;;; lispm-1password.el --- 1Password's vaults, through its command-line tool  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-1password
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: tools, convenience
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The vaults of 1Password, through op, 1Password's command-line tool.
;; M-x lispm-1password opens its window: the items of the vaults, one a
;; line, searched as words are typed; an item opened shows its fields.
;; Usernames, passwords and one-time codes are copied to the clipboard,
;; and a secret copied leaves it again after 90 seconds.
;;
;; Secrets are read only when they are copied or shown.  What op says
;; of an item is kept without them, a secret shown is concealed again
;; after 30 seconds, and nothing is written to disk.
;;
;; op asks the 1Password app to unlock, with Touch ID or the app's
;; password, once Settings > Developer > Integrate with 1Password CLI
;; is on there.  Without the app, an account added with `op account
;; add' is unlocked with its password, asked in the minibuffer and
;; handed to op, never kept.
;;
;; This file runs op and keeps what it said; lispm-1password-ui.el is
;; the window.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url-util)

(defgroup lispm-1password nil
  "1Password's vaults, through its command-line tool."
  :group 'applications
  :prefix "lispm-1password-")

(defcustom lispm-1password-program "op"
  "1Password's command-line tool: its name on `exec-path', or its file."
  :type 'string)

(defcustom lispm-1password-account nil
  "The account op uses: its sign-in address, email or ID.
nil uses the first account op knows."
  :type '(choice (const :tag "The first" nil) string))

(defcustom lispm-1password-clear-seconds 90
  "Seconds a secret copied stays on the clipboard; nil leaves it there."
  :type '(choice (const :tag "Never cleared" nil) natnum))

(defcustom lispm-1password-reveal-seconds 30
  "Seconds a secret shown stays shown; nil, until it is concealed."
  :type '(choice (const :tag "Until concealed" nil) natnum))

(defcustom lispm-1password-password-recipe "letters,digits,symbols,32"
  "How op generates the password of a new login, as its option
--generate-password takes it: the characters it uses, and its length."
  :type 'string)

(defcustom lispm-1password-install-command
  (when (eq system-type 'darwin) "brew install --cask 1password-cli")
  "The command installing op, shown while it is missing; nil for none."
  :type '(choice (const :tag "None" nil) string))

(defconst lispm-1password-install-page "https://developer.1password.com/docs/cli/get-started/"
  "1Password's page on installing op.")

;;;; What 1Password is

(defvar lispm-1password-state nil
  "Where 1Password stands: nil before it is opened, `missing' without op,
`no-account' while op knows no account, `locked', `unlocking' and
`loading' while op works, `ready' once the items are read, or `error'.")

(defvar lispm-1password-message nil
  "What op said when it last failed, for the window to show.")

(defvar lispm-1password-accounts nil
  "The accounts op knows, as it lists them: plists of :url, :email,
:user_uuid and :account_uuid.")

(defvar lispm-1password--account nil
  "The account in use, one of `lispm-1password-accounts', or nil.")

(defvar lispm-1password-vaults nil
  "The account's vaults: plists of :id and :name.")

(defvar lispm-1password-items nil
  "The items of the vaults, without their secrets: see `lispm-1password--item'.")

(defvar lispm-1password-read-at nil
  "When the items were read.")

(defvar lispm-1password--session nil
  "op's session token, when unlocked with the account's password rather
than through the 1Password app; kept in memory only.")

(defvar lispm-1password-changed-hook nil
  "Hook run when what 1Password shows changed: its state or its items.")

(defun lispm-1password--set-state (state)
  "Say that 1Password stands at STATE, and show it."
  (setq lispm-1password-state state)
  (run-hooks 'lispm-1password-changed-hook))

(defun lispm-1password-account-name ()
  "Return the email of the account in use, or nil."
  (plist-get lispm-1password--account :email))

;;;; Running op

(defun lispm-1password-installed-p ()
  "Return op's file, or nil when it is not installed."
  (executable-find lispm-1password-program))

(defun lispm-1password--command (arguments)
  "Return the command running op with ARGUMENTS for the account in use."
  (append (list (or (lispm-1password-installed-p) lispm-1password-program))
          arguments
          (when-let* ((account (or lispm-1password-account
                                   (plist-get lispm-1password--account :account_uuid))))
            (list "--account" account))
          (when lispm-1password--session
            (list "--session" lispm-1password--session))))

(defun lispm-1password--said (text)
  "Return what op said in TEXT, its error output, without its labels."
  (let ((said (string-join
               (mapcar (lambda (line)
                         (replace-regexp-in-string
                          "\\`\\[[A-Z]+\\] \\(?:[0-9/]+ [0-9:]+ \\)?" "" line))
                       (split-string text "[\r\n]+" t "[ \t]+"))
               "\n")))
    (if (string-empty-p said)
        said
      (concat (upcase (substring said 0 1)) (substring said 1)))))

(defconst lispm-1password--locked-regexp
  (regexp-opt '("not currently signed in" "not signed in" "session expired" "op signin"
                "authorization prompt dismissed" "authorization timeout" "authorization denied"
                "is locked"))
  "What op says when it is to be unlocked first.")

(defun lispm-1password-locked-error-p (said)
  "Return non-nil when op, saying SAID, failed for being locked."
  (let ((case-fold-search t))
    (string-match-p lispm-1password--locked-regexp said)))

(defun lispm-1password--run (arguments done &optional failed input)
  "Run op with ARGUMENTS, then call DONE with what it printed.
When op fails, call FAILED with what it said, by default
`lispm-1password--action-failed'.  INPUT, a string, is handed to op on
its standard input, which is closed after it."
  (let* ((stdout (generate-new-buffer " *lispm-1password*" t))
         (stderr (generate-new-buffer " *lispm-1password errors*" t))
         (left 2)
         (status nil)
         (finish
          (lambda ()
            (when (zerop (cl-decf left))
              (let ((output (with-current-buffer stdout (buffer-string)))
                    (said (lispm-1password--said (with-current-buffer stderr (buffer-string)))))
                (kill-buffer stdout)
                (kill-buffer stderr)
                (if (eql status 0)
                    (funcall done output)
                  (funcall (or failed #'lispm-1password--action-failed)
                           (if (string-empty-p said) (format "op failed, with status %s" status) said)))))))
         (errors (make-pipe-process :name "lispm-1password errors" :buffer stderr :noquery t
                                    :coding 'utf-8
                                    :sentinel (lambda (process _event)
                                                (unless (process-live-p process)
                                                  (funcall finish)))))
         (process (make-process :name "lispm-1password" :buffer stdout :noquery t
                                :connection-type 'pipe :coding 'utf-8
                                :command (lispm-1password--command arguments)
                                :stderr errors
                                :sentinel (lambda (process _event)
                                            (unless (process-live-p process)
                                              (setq status (process-exit-status process))
                                              (funcall finish))))))
    (when input
      (process-send-string process input))
    (process-send-eof process)
    process))

(defun lispm-1password--json (text)
  "Return the JSON TEXT op printed, objects as plists, or nil for none."
  (unless (string-blank-p text)
    (json-parse-string text :object-type 'plist :array-type 'list
                       :null-object nil :false-object nil)))

(defun lispm-1password--fail (said)
  "Note that op failed to read the vaults, saying SAID."
  (setq lispm-1password-message said
        lispm-1password-items nil)
  (lispm-1password--set-state (if (lispm-1password-locked-error-p said) 'locked 'error)))

(defun lispm-1password--action-failed (said)
  "Say that what op was asked to do failed, saying SAID."
  (message "1Password: %s" said))

;;;; Items

(defconst lispm-1password-secret-types '("CONCEALED" "OTP" "SSHKEY" "CREDIT_CARD_NUMBER")
  "The types of fields whose values are secrets, read only when used.")

(defun lispm-1password--forget (value)
  "Blank out VALUE, a string op said of a secret, and return nil."
  (when (stringp value)
    (clear-string value))
  nil)

(defun lispm-1password--field (json)
  "Return the field op described in JSON as a plist, its secret left out:
:id, :label, :type, :purpose, :section, :value, :secret, :empty,
:strength."
  (let* ((type (or (plist-get json :type) "STRING"))
         (secret (or (member type lispm-1password-secret-types)
                     (equal (plist-get json :purpose) "PASSWORD")))
         (value (plist-get json :value))
         (field (list :id (plist-get json :id)
                      :label (or (plist-get json :label) (plist-get json :id) "")
                      :type type
                      :purpose (plist-get json :purpose)
                      :section (plist-get (plist-get json :section) :id)
                      :value (unless secret value)
                      :secret (and secret t)
                      ;; A secret op leaves out may still be there.
                      :empty (and (plist-member json :value) (equal value "") (not (plist-get json :totp)))
                      :strength (plist-get (plist-get json :password_details) :strength))))
    (when secret
      (lispm-1password--forget value)
      (lispm-1password--forget (plist-get json :totp)))
    field))

(defun lispm-1password--urls (urls)
  "Return the addresses of URLS, as op lists them, the primary one first."
  (mapcar (lambda (url) (plist-get url :href))
          (seq-sort-by (lambda (url) (if (plist-get url :primary) 0 1)) #'< urls)))

(defun lispm-1password--item (json)
  "Return the item op described in JSON as a plist, its secrets left out:
:id, :title, :vault-id, :vault, :category, :details, :urls, :favorite,
:tags, :created and :updated; and once read whole, :whole t, :fields and
:sections."
  (let ((vault (plist-get json :vault)))
    `(:id ,(plist-get json :id)
      :title ,(or (plist-get json :title) "")
      :vault-id ,(plist-get vault :id)
      :vault ,(or (plist-get vault :name) "")
      :category ,(or (plist-get json :category) "")
      :details ,(or (plist-get json :additional_information) "")
      :urls ,(lispm-1password--urls (plist-get json :urls))
      :favorite ,(and (plist-get json :favorite) t)
      :tags ,(plist-get json :tags)
      :created ,(plist-get json :created_at)
      :updated ,(plist-get json :updated_at)
      ,@(when (plist-member json :fields)
          `(:whole t
            :fields ,(mapcar #'lispm-1password--field (plist-get json :fields))
            :sections ,(mapcar (lambda (section)
                                 (list :id (plist-get section :id) :label (plist-get section :label)))
                               (plist-get json :sections)))))))

(defun lispm-1password--item-of (output)
  "Return the item op printed in OUTPUT, which is then blanked out."
  (prog1 (lispm-1password--item (lispm-1password--json output))
    (clear-string output)))

(defun lispm-1password-find (id)
  "Return the item of ID, or nil."
  (seq-find (lambda (item) (equal (plist-get item :id) id)) lispm-1password-items))

(defun lispm-1password--keep (item)
  "Keep ITEM in place of the item of its ID, or with the items if new."
  (let ((cell (seq-position lispm-1password-items (plist-get item :id)
                            (lambda (old id) (equal (plist-get old :id) id)))))
    (if cell
        (setf (nth cell lispm-1password-items) item)
      (setq lispm-1password-items (append lispm-1password-items (list item)))))
  (run-hooks 'lispm-1password-changed-hook)
  item)

(defun lispm-1password-field (item test)
  "Return the first field of ITEM, read whole, for which TEST returns non-nil."
  (seq-find test (plist-get item :fields)))

(defun lispm-1password-username-field (item)
  "Return ITEM's username field, or nil."
  (lispm-1password-field item (lambda (field) (equal (plist-get field :purpose) "USERNAME"))))

(defun lispm-1password-password-field (item)
  "Return ITEM's password field, or its first secret one, or nil."
  (or (lispm-1password-field item (lambda (field) (equal (plist-get field :purpose) "PASSWORD")))
      (lispm-1password-field item (lambda (field) (equal (plist-get field :type) "CONCEALED")))))

(defun lispm-1password-code-field (item)
  "Return ITEM's one-time password field, or nil."
  (lispm-1password-field item (lambda (field) (equal (plist-get field :type) "OTP"))))

;;;; Opening the vaults

(defun lispm-1password--choose-account ()
  "Return the account to use, of `lispm-1password-accounts': the one in
use, the one `lispm-1password-account' names, or the first."
  (or (seq-find (lambda (account)
                  (equal (plist-get account :account_uuid) (plist-get lispm-1password--account :account_uuid)))
                lispm-1password-accounts)
      (and lispm-1password-account
           (seq-find (lambda (account)
                       (member lispm-1password-account
                               (list (plist-get account :url) (plist-get account :email)
                                     (plist-get account :account_uuid) (plist-get account :user_uuid)
                                     (plist-get account :shorthand))))
                     lispm-1password-accounts))
      (car lispm-1password-accounts)))

(defun lispm-1password-reload ()
  "Read the vaults and their items.
Once Integrate with 1Password CLI is on in the 1Password app, op asks
the app to unlock first if need be."
  (interactive)
  (cond ((not (lispm-1password-installed-p))
         (lispm-1password--set-state 'missing))
        ((memq lispm-1password-state '(loading unlocking)))
        (t
         (lispm-1password--set-state 'loading)
         (lispm-1password--run
          '("account" "list" "--format" "json")
          (lambda (output)
            (setq lispm-1password-accounts (lispm-1password--json output)
                  lispm-1password--account (lispm-1password--choose-account))
            (if (and (null lispm-1password-accounts) (not (getenv "OP_SERVICE_ACCOUNT_TOKEN")))
                (progn (setq lispm-1password-message nil)
                       (lispm-1password--set-state 'no-account))
              (lispm-1password--read-vaults)))
          #'lispm-1password--fail))))

(defun lispm-1password--read-vaults ()
  "Read the vaults, then their items."
  (lispm-1password--run
   '("vault" "list" "--format" "json")
   (lambda (output)
     (setq lispm-1password-vaults
           (mapcar (lambda (vault) (list :id (plist-get vault :id) :name (plist-get vault :name)))
                   (lispm-1password--json output)))
     (lispm-1password--run
      '("item" "list" "--format" "json")
      (lambda (output)
        (setq lispm-1password-items (mapcar #'lispm-1password--still-whole
                                            (mapcar #'lispm-1password--item (lispm-1password--json output)))
              lispm-1password-read-at (current-time)
              lispm-1password-message nil)
        (lispm-1password--set-state 'ready))
      #'lispm-1password--fail))
   #'lispm-1password--fail))

(defun lispm-1password--still-whole (item)
  "Return ITEM, as op lists it, with the fields read of it before, when it
has not changed since."
  (let ((old (lispm-1password-find (plist-get item :id))))
    (if (and (plist-get old :whole) (equal (plist-get old :updated) (plist-get item :updated)))
        (append item (list :whole t :fields (plist-get old :fields) :sections (plist-get old :sections)))
      item)))

(defconst lispm-1password--cancelled "Unlocking was cancelled."
  "What the window says when the password op asked for was not given.")

(defconst lispm-1password--password-prompt "\\([^\n]*password[^\n]*:\\)[ \t]*\\'"
  "What op asks a password with, at the end of what it printed.")

(defun lispm-1password-unlock ()
  "Unlock 1Password, and read the vaults.
op asks the 1Password app, which asks for Touch ID or its password;
without the app, the account's password is asked here and handed to op."
  (interactive)
  (cond ((not (lispm-1password-installed-p))
         (lispm-1password--set-state 'missing))
        ((memq lispm-1password-state '(loading unlocking)))
        (t
         (setq lispm-1password--session nil
               lispm-1password-message nil)
         (lispm-1password--set-state 'unlocking)
         (let ((output ""))
           (make-process
            :name "lispm-1password signin" :noquery t :connection-type 'pty :coding 'utf-8
            :command (lispm-1password--command '("signin" "--raw"))
            :filter (lambda (process text)
                      (setq output (concat output (string-replace "\r" "" text)))
                      (when (let ((case-fold-search t))
                              (string-match lispm-1password--password-prompt output))
                        (let ((prompt (match-string 1 output)))
                          (setq output "")
                          (run-at-time 0 nil #'lispm-1password--answer process prompt))))
            :sentinel (lambda (process _event)
                        (unless (process-live-p process)
                          (lispm-1password--signed-in (process-exit-status process) output))))))))

(defun lispm-1password--answer (process prompt)
  "Ask the password op asks for with PROMPT, and hand it to PROCESS."
  (when (process-live-p process)
    (if-let* ((password (condition-case nil
                            (read-passwd (concat (string-trim prompt) " "))
                          (quit nil))))
        (progn (process-send-string process (concat password "\n"))
               (clear-string password))
      (setq lispm-1password-message lispm-1password--cancelled)
      (delete-process process))))

(defun lispm-1password--signed-in (status output)
  "Go on once op signin ended with STATUS, having printed OUTPUT."
  (let ((lines (split-string output "\n" t "[ \t]+")))
    (if (eql status 0)
        (let ((token (car (last lines))))
          ;; Through the app, op prints no token.
          (when (and token (string-match-p "\\`[A-Za-z0-9_-]\\{20,\\}\\'" token))
            (setq lispm-1password--session token))
          (setq lispm-1password-state nil)
          (lispm-1password-reload))
      (unless (equal lispm-1password-message lispm-1password--cancelled)
        (setq lispm-1password-message (lispm-1password--said (string-join lines "\n"))))
      (lispm-1password--set-state 'locked))))

(defun lispm-1password-lock ()
  "Lock 1Password: sign op out, and forget the items read."
  (interactive)
  (when (lispm-1password-installed-p)
    (lispm-1password--run '("signout") #'ignore #'ignore))
  (setq lispm-1password--session nil
        lispm-1password-items nil
        lispm-1password-vaults nil
        lispm-1password-read-at nil
        lispm-1password-message nil)
  (lispm-1password--set-state 'locked))

;;;; Reading and changing items

(defun lispm-1password-read-item (item done &optional failed)
  "Read ITEM whole, without its secrets, keep it, and call DONE with it.
When op fails, call FAILED with what it said, see `lispm-1password--run'."
  (lispm-1password--run
   (list "item" "get" (plist-get item :id) "--vault" (plist-get item :vault-id) "--format" "json")
   (lambda (output)
     (funcall done (lispm-1password--keep (lispm-1password--item-of output))))
   failed))

(defun lispm-1password-whole (item done)
  "Call DONE with ITEM read whole, reading it first if need be."
  (if (plist-get item :whole)
      (funcall done item)
    (lispm-1password-read-item item done)))

(defun lispm-1password--reference (item field &optional code)
  "Return op's secret reference to FIELD of ITEM, by their IDs; to its
one-time code with CODE."
  (concat "op://" (plist-get item :vault-id) "/" (plist-get item :id) "/"
          (when-let* ((section (plist-get field :section)))
            (concat section "/"))
          (plist-get field :id)
          (when code "?attribute=otp")))

(defun lispm-1password-read-secret (item field done)
  "Read the secret of FIELD of ITEM, its current code for a one-time
password, and call DONE with it."
  (lispm-1password--run
   (list "read" "--no-newline"
         (lispm-1password--reference item field (equal (plist-get field :type) "OTP")))
   (lambda (output)
     (funcall done (string-remove-suffix "\n" output)))))

(defun lispm-1password-create-login (title vault username url password done)
  "Create a login TITLE in the vault of ID VAULT, for USERNAME at URL, and
call DONE with it.  PASSWORD nil has op generate one, see
`lispm-1password-password-recipe'.  The password goes to op on its
standard input, never on its command line."
  (let ((template (json-serialize
                   `(:title ,title
                     :category "LOGIN"
                     :fields [(:id "username" :type "STRING" :purpose "USERNAME" :label "username"
                               :value ,(or username ""))
                              ,@(when password
                                  `((:id "password" :type "CONCEALED" :purpose "PASSWORD" :label "password"
                                     :value ,password)))]))))
    (lispm-1password--run
     `("item" "create" "--vault" ,vault "--format" "json"
       ,@(unless (string-empty-p (or url "")) (list "--url" url))
       ,@(unless password (list (concat "--generate-password=" lispm-1password-password-recipe)))
       "-")
     (lambda (output)
       (funcall done (lispm-1password--keep (lispm-1password--item-of output))))
     nil template)
    (clear-string template)))

(defun lispm-1password-set-favorite (item favorite)
  "Make ITEM a favorite, or with FAVORITE nil no longer."
  (lispm-1password--run
   (list "item" "edit" (plist-get item :id) "--vault" (plist-get item :vault-id)
         (if favorite "--favorite=true" "--favorite=false") "--format" "json")
   (lambda (output)
     (lispm-1password--keep (lispm-1password--item-of output)))))

(defun lispm-1password-archive-item (item done)
  "Move ITEM to the Archive, and call DONE."
  (lispm-1password--run
   (list "item" "delete" (plist-get item :id) "--vault" (plist-get item :vault-id) "--archive")
   (lambda (_output)
     (setq lispm-1password-items (seq-remove (lambda (old) (equal (plist-get old :id) (plist-get item :id)))
                                             lispm-1password-items))
     (run-hooks 'lispm-1password-changed-hook)
     (funcall done))))

(defun lispm-1password-link (item)
  "Return the link opening ITEM in the 1Password app, as its Copy Private
Link gives it."
  (format "https://start.1password.com/open/i?a=%s&v=%s&i=%s&h=%s"
          (url-hexify-string (or (plist-get lispm-1password--account :account_uuid) ""))
          (url-hexify-string (plist-get item :vault-id))
          (url-hexify-string (plist-get item :id))
          (url-hexify-string (or (plist-get lispm-1password--account :url) "my.1password.com"))))

;;;; The clipboard

(defvar lispm-1password--clipboard nil
  "The secret copied last, still to be cleared: (HASH . TIMER).")

(defun lispm-1password--clipboard-set (text)
  "Put TEXT on the system's clipboard, not on the kill ring."
  (if interprogram-cut-function
      (funcall interprogram-cut-function text)
    (kill-new text)))

(defun lispm-1password--clipboard-get ()
  "Return what the system's clipboard holds, or nil."
  (ignore-errors (gui-get-selection 'CLIPBOARD 'UTF8_STRING)))

(defun lispm-1password-copy (text what &optional secret)
  "Copy TEXT, WHAT the message calls it, to the clipboard.
A SECRET leaves it after `lispm-1password-clear-seconds', unless
something else was copied meanwhile."
  (when lispm-1password--clipboard
    (cancel-timer (cdr lispm-1password--clipboard))
    (setq lispm-1password--clipboard nil))
  ;; A copy: what Emacs put on the clipboard it keeps, and a secret is
  ;; blanked out once copied.
  (lispm-1password--clipboard-set (copy-sequence text))
  (if (and secret lispm-1password-clear-seconds)
      (let ((hash (secure-hash 'sha256 text)))
        (setq lispm-1password--clipboard
              (cons hash (run-at-time lispm-1password-clear-seconds nil #'lispm-1password-clear-clipboard)))
        (message "Copied %s; it leaves the clipboard in %d seconds" what lispm-1password-clear-seconds))
    (message "Copied %s" what)))

(defun lispm-1password-clear-clipboard ()
  "Clear the secret copied last from the clipboard, if it still holds it."
  (when-let* ((copied lispm-1password--clipboard))
    (cancel-timer (cdr copied))
    (setq lispm-1password--clipboard nil)
    (when (equal (secure-hash 'sha256 (or (lispm-1password--clipboard-get) "")) (car copied))
      (lispm-1password--clipboard-set ""))))

;; A secret copied does not outlive Emacs on the clipboard.
(add-hook 'kill-emacs-hook #'lispm-1password-clear-clipboard)

(defun lispm-1password-copy-field (item field)
  "Copy FIELD of ITEM, reading it first if it is a secret."
  (let ((what (plist-get field :label)))
    (if (plist-get field :secret)
        (lispm-1password-read-secret item field
                                     (lambda (secret)
                                       (lispm-1password-copy secret what t)
                                       (clear-string secret)))
      (lispm-1password-copy (or (plist-get field :value) "") what))))

(provide 'lispm-1password)

;;; lispm-1password.el ends here
