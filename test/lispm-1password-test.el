;;; lispm-1password-test.el --- Tests of 1Password  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

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

;; The tests run test/fake-op, which answers as op does, in place of op:
;; no vault is read, and nothing is signed in to.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lispm-1password)
(require 'lispm-1password-ui)

(defconst lispm-1password-test--op
  (expand-file-name "fake-op" (file-name-directory (or load-file-name buffer-file-name)))
  "The stand-in for op.")

(defvar lispm-1password-test--log nil
  "The file the stand-in for op writes its calls to.")

(defvar lispm-1password-test--stdin nil
  "The file the stand-in for op writes what it was handed to.")

(defmacro lispm-1password-test--with-op (mode &rest body)
  "Run BODY with the stand-in for op, in MODE, and 1Password as new."
  (declare (indent 1))
  `(let* ((lispm-1password-test--log (make-temp-file "lispm-1password-log"))
          (lispm-1password-test--stdin (make-temp-file "lispm-1password-stdin"))
          (process-environment (append (list (format "FAKE_OP_MODE=%s" (or ,mode ""))
                                             (concat "FAKE_OP_LOG=" lispm-1password-test--log)
                                             (concat "FAKE_OP_STDIN=" lispm-1password-test--stdin))
                                       process-environment))
          (lispm-1password-program lispm-1password-test--op)
          (lispm-1password-account nil)
          (lispm-1password-state nil)
          (lispm-1password-message nil)
          (lispm-1password-accounts nil)
          (lispm-1password--account nil)
          (lispm-1password-vaults nil)
          (lispm-1password-items nil)
          (lispm-1password-read-at nil)
          (lispm-1password--session nil)
          (lispm-1password--clipboard nil)
          (lispm-1password--vault nil)
          (lispm-1password--category nil)
          (lispm-1password--favorites nil)
          (lispm-1password--search ""))
     (set-file-modes lispm-1password-test--op #o755)
     (unwind-protect
         (progn ,@body)
       (dolist (name (list lispm-1password-buffer-name lispm-1password-item-buffer-name))
         (when (get-buffer name)
           (kill-buffer name)))
       (when lispm-1password--clipboard
         (cancel-timer (cdr lispm-1password--clipboard)))
       (delete-file lispm-1password-test--log)
       (delete-file lispm-1password-test--stdin))))

(defun lispm-1password-test--wait (predicate)
  "Wait for PREDICATE to return non-nil, as op answers; then check it does."
  (let ((deadline (+ (float-time) 10)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05)))
  (should (funcall predicate)))

(defun lispm-1password-test--calls ()
  "Return the calls of the stand-in for op, one a string."
  (with-temp-buffer
    (insert-file-contents lispm-1password-test--log)
    (split-string (buffer-string) "\n" t)))

(defun lispm-1password-test--open ()
  "Read the vaults, and wait until they are."
  (lispm-1password-reload)
  (lispm-1password-test--wait (lambda () (eq lispm-1password-state 'ready))))

(defun lispm-1password-test--read (id)
  "Read the item of ID whole, and return it once read."
  (let ((read nil))
    (lispm-1password-read-item (lispm-1password-find id) (lambda (item) (setq read item)))
    (lispm-1password-test--wait (lambda () read))
    read))

(defmacro lispm-1password-test--with-clipboard (&rest body)
  "Run BODY with a clipboard of its own, the variable `clipboard'."
  (declare (indent 0))
  `(let ((clipboard nil))
     (cl-letf (((symbol-function 'lispm-1password--clipboard-set) (lambda (text) (setq clipboard text)))
               ((symbol-function 'lispm-1password--clipboard-get) (lambda () clipboard)))
       ,@body)))

;;;; op

(ert-deftest lispm-1password-test-said ()
  "What op says loses its labels, and starts with a capital."
  (should (equal (lispm-1password--said
                  "[ERROR] 2026/10/21 12:00:00 you are not currently signed in.\n")
                 "You are not currently signed in."))
  (should (equal (lispm-1password--said "") ""))
  (should (lispm-1password-locked-error-p "You are not currently signed in. Please run `op signin`"))
  (should (lispm-1password-locked-error-p "authorization prompt dismissed, please try again"))
  (should-not (lispm-1password-locked-error-p "\"x\" isn't an item.")))

(ert-deftest lispm-1password-test-missing ()
  "Without op, 1Password says how to install it."
  (lispm-1password-test--with-op nil
    (let ((lispm-1password-program "lispm-1password-no-such-op")
          (lispm-1password-install-command "brew install 1password-cli"))
      (lispm-1password-reload)
      (should (eq lispm-1password-state 'missing))
      (should (lispm-1password--page-p))
      (should (string-search "brew install 1password-cli" (lispm-1password--page-text)))
      (should (assoc "Copy Command" (lispm-1password--page-buttons))))))

(ert-deftest lispm-1password-test-open ()
  "The vaults and their items are read, for the first account."
  (lispm-1password-test--with-op nil
    (lispm-1password-test--open)
    (should (equal (lispm-1password-account-name) "ada@example.com"))
    (should (equal (mapcar (lambda (vault) (plist-get vault :name)) lispm-1password-vaults) '("Personal" "Work")))
    (should (= (length lispm-1password-items) 4))
    (let ((github (lispm-1password-find "ITEM1"))
          (bank (lispm-1password-find "ITEM2")))
      (should (equal (plist-get github :vault) "Personal"))
      (should (equal (plist-get github :details) "ada"))
      (should (plist-get github :favorite))
      (should-not (plist-get github :whole))
      (should-not (plist-get bank :favorite))
      ;; The primary website first.
      (should (equal (plist-get bank :urls) '("https://www.bank.example/login" "https://bank.example"))))
    ;; Once known, the account is named to op.
    (should (string-match-p "\\`item list --format json --account ACCOUNT1\\'"
                            (car (last (lispm-1password-test--calls)))))
    (should-not (lispm-1password--page-p))))

(ert-deftest lispm-1password-test-no-account ()
  "Without an account, the page says to turn on the app's integration."
  (lispm-1password-test--with-op "no-account"
    (lispm-1password-reload)
    (lispm-1password-test--wait (lambda () (eq lispm-1password-state 'no-account)))
    (should (string-search "Integrate with 1Password CLI" (lispm-1password--page-text)))
    (should (assoc "Open 1Password" (lispm-1password--page-buttons)))))

(ert-deftest lispm-1password-test-locked ()
  "op to be signed in to leaves 1Password locked, with Unlock."
  (lispm-1password-test--with-op "locked"
    (lispm-1password-reload)
    (lispm-1password-test--wait (lambda () (eq lispm-1password-state 'locked)))
    (should (string-prefix-p "You are not currently signed in" lispm-1password-message))
    (should (string-prefix-p "1Password is locked." (lispm-1password--page-text)))
    (should (equal (lispm-1password--page-buttons) '(("Unlock" lispm-1password-unlock))))))

(ert-deftest lispm-1password-test-unlock-with-password ()
  "Without the app, the password is asked and handed to op, which gives
the session the rest is read in."
  (lispm-1password-test--with-op "password"
    (lispm-1password-reload)
    (lispm-1password-test--wait (lambda () (eq lispm-1password-state 'locked)))
    (let ((asked nil))
      (cl-letf (((symbol-function 'read-passwd)
                 (lambda (prompt &rest _) (setq asked prompt) (copy-sequence "correct horse"))))
        (lispm-1password-unlock)
        (lispm-1password-test--wait (lambda () (eq lispm-1password-state 'ready))))
      (should (string-match-p "Enter the password for ada@example.com" asked)))
    (should (equal lispm-1password--session "TOKENtokenTOKENtoken123"))
    (should (= (length lispm-1password-items) 4))
    ;; The password never is on op's command line.
    (should-not (seq-some (lambda (call) (string-search "correct horse" call)) (lispm-1password-test--calls)))
    (lispm-1password-lock)
    (should (eq lispm-1password-state 'locked))
    (should-not lispm-1password--session)
    (should-not lispm-1password-items)))

(ert-deftest lispm-1password-test-unlock-cancelled ()
  "A password not given leaves 1Password locked, saying so."
  (lispm-1password-test--with-op "password"
    (lispm-1password-reload)
    (lispm-1password-test--wait (lambda () (eq lispm-1password-state 'locked)))
    (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) (signal 'quit nil))))
      (lispm-1password-unlock)
      (lispm-1password-test--wait (lambda () (eq lispm-1password-state 'locked))))
    (should (equal lispm-1password-message "Unlocking was cancelled."))))

;;;; Items and their secrets

(ert-deftest lispm-1password-test-item-without-secrets ()
  "An item read whole keeps its fields, but none of its secrets."
  (lispm-1password-test--with-op nil
    (lispm-1password-test--open)
    (let* ((item (lispm-1password-test--read "ITEM1"))
           (printed (format "%S" lispm-1password-items)))
      (should (plist-get item :whole))
      (dolist (secret '("hunter2" "JBSWY3DPEHPK3PXP" "123456" "abc-def"))
        (should-not (string-search secret printed)))
      (should (equal (plist-get (lispm-1password-username-field item) :value) "ada"))
      (let ((password (lispm-1password-password-field item)))
        (should (plist-get password :secret))
        (should-not (plist-get password :value))
        (should-not (plist-get password :empty))
        (should (equal (plist-get password :strength) "VERY_GOOD")))
      (should (equal (plist-get (lispm-1password-code-field item) :section) "add more"))
      (should (plist-get (lispm-1password-field item (lambda (field) (equal (plist-get field :id) "empty1")))
                         :empty))
      (should (equal (plist-get (lispm-1password-field item (lambda (field) (equal (plist-get field :purpose) "NOTES")))
                                :value)
                     "Two keys, one in the safe."))
      (should (equal (mapcar (lambda (section) (plist-get section :label)) (plist-get item :sections))
                     '(nil "Recovery"))))))

(ert-deftest lispm-1password-test-references ()
  "Secrets are read by IDs, in their sections, codes as codes."
  (let ((item '(:id "I" :vault-id "V")))
    (should (equal (lispm-1password--reference item '(:id "password")) "op://V/I/password"))
    (should (equal (lispm-1password--reference item '(:id "TOTP1" :section "add more") t)
                   "op://V/I/add more/TOTP1?attribute=otp"))))

(ert-deftest lispm-1password-test-copy-and-clear ()
  "A password copied leaves the clipboard again, if it still holds it."
  (lispm-1password-test--with-op nil
    (lispm-1password-test--with-clipboard
      (lispm-1password-test--open)
      (let ((item (lispm-1password-test--read "ITEM1")))
        (lispm-1password-copy-field item (lispm-1password-password-field item))
        (lispm-1password-test--wait (lambda () (equal clipboard "hunter2")))
        (should (string-search "read --no-newline op://VAULT1/ITEM1/password"
                               (car (last (lispm-1password-test--calls)))))
        (should lispm-1password--clipboard)
        (lispm-1password-clear-clipboard)
        (should (equal clipboard ""))
        (should-not lispm-1password--clipboard)
        ;; Something else copied meanwhile stays.
        (lispm-1password-copy-field item (lispm-1password-password-field item))
        (lispm-1password-test--wait (lambda () (equal clipboard "hunter2")))
        (setq clipboard "copied since")
        (lispm-1password-clear-clipboard)
        (should (equal clipboard "copied since"))
        ;; A username is no secret: nothing to clear.
        (lispm-1password-copy-field item (lispm-1password-username-field item))
        (should (equal clipboard "ada"))
        (should-not lispm-1password--clipboard)))))

(ert-deftest lispm-1password-test-copy-code-from-list ()
  "The one-time code is copied from the list, the item read first."
  (lispm-1password-test--with-op nil
    (lispm-1password-test--with-clipboard
      (lispm-1password-test--open)
      (with-current-buffer (lispm-1password--list-buffer)
        (tabulated-list-print)
        (goto-char (point-min))
        (while (not (equal (tabulated-list-get-id) "ITEM1"))
          (forward-line 1))
        (lispm-1password-copy-code)
        (lispm-1password-test--wait (lambda () (equal clipboard "123456")))
        (should (plist-get (lispm-1password-find "ITEM1") :whole))))))

(ert-deftest lispm-1password-test-create-login ()
  "A login's password goes to op on its standard input; or op generates
one, which is copied."
  (lispm-1password-test--with-op nil
    (lispm-1password-test--with-clipboard
      (lispm-1password-test--open)
      (let ((made nil))
        (lispm-1password-create-login "Forum" "VAULT2" "ada" "https://forum.example" (copy-sequence "pa55 word")
                                      (lambda (item) (setq made item)))
        (lispm-1password-test--wait (lambda () made))
        (let ((call (car (last (lispm-1password-test--calls))))
              (template (with-temp-buffer
                          (insert-file-contents lispm-1password-test--stdin)
                          (json-parse-buffer :object-type 'plist :array-type 'list))))
          (should (string-prefix-p "item create --vault VAULT2 --format json --url https://forum.example -" call))
          (should-not (string-search "pa55" call))
          (should (equal (plist-get template :title) "Forum"))
          (should (equal (mapcar (lambda (field) (plist-get field :value)) (plist-get template :fields))
                         '("ada" "pa55 word")))))
      (should (lispm-1password-find "NEW1"))
      (should-not (string-search "G3n3rat3d" (format "%S" lispm-1password-items)))
      (let ((window (selected-window)))
        (lispm-1password-new "Forum" "VAULT2" "ada" "" nil)
        (lispm-1password-test--wait (lambda () (equal clipboard "G3n3rat3d!")))
        (should (string-search "--generate-password=letters,digits,symbols,32"
                               (seq-find (lambda (call) (string-prefix-p "item create" call))
                                         (reverse (lispm-1password-test--calls)))))
        (should (equal (buffer-name (window-buffer window)) lispm-1password-item-buffer-name))))))

(ert-deftest lispm-1password-test-favorite-and-archive ()
  "Favorites are set through op; an item archived leaves the list."
  (lispm-1password-test--with-op nil
    (lispm-1password-test--open)
    (lispm-1password-set-favorite (lispm-1password-find "ITEM1") nil)
    (lispm-1password-test--wait (lambda () (not (plist-get (lispm-1password-find "ITEM1") :favorite))))
    (should (string-search "--favorite=false" (car (last (lispm-1password-test--calls)))))
    (let ((done nil))
      (lispm-1password-archive-item (lispm-1password-find "ITEM1") (lambda () (setq done t)))
      (lispm-1password-test--wait (lambda () done)))
    (should (string-prefix-p "item delete ITEM1 --vault VAULT1 --archive" (car (last (lispm-1password-test--calls)))))
    (should-not (lispm-1password-find "ITEM1"))
    (should (= (length lispm-1password-items) 3))))

(ert-deftest lispm-1password-test-reload-keeps-fields ()
  "Read again, an item unchanged keeps the fields read of it."
  (lispm-1password-test--with-op nil
    (lispm-1password-test--open)
    (lispm-1password-test--read "ITEM1")
    (setq lispm-1password-state 'ready)
    (lispm-1password-reload)
    (lispm-1password-test--wait (lambda () (eq lispm-1password-state 'ready)))
    (should (plist-get (lispm-1password-find "ITEM1") :whole))
    (should (plist-get (lispm-1password-find "ITEM1") :favorite))))

;;;; The window

(defun lispm-1password-test--titles ()
  "Return the titles the list shows, in its order."
  (with-current-buffer (lispm-1password--list-buffer)
    (tabulated-list-print)
    (goto-char (point-min))
    (let ((titles nil))
      (while (not (eobp))
        (when-let* ((entry (tabulated-list-get-entry)))
          (push (aref entry 1) titles))
        (forward-line 1))
      (nreverse titles))))

(ert-deftest lispm-1password-test-list ()
  "The list is by title in any case, and narrowed by vault, kind,
favorites and words."
  (lispm-1password-test--with-op nil
    (lispm-1password-test--open)
    (should (equal (lispm-1password-test--titles) '("bank" "GitHub" "Home Wi-Fi" "Server notes")))
    (should-not (lispm-1password--filter-label))
    (with-current-buffer (lispm-1password--list-buffer)
      (should (equal (lispm-1password--status) '("4 items" "ada@example.com"))))
    (lispm-1password-show-vault "VAULT2")
    (should (equal (lispm-1password-test--titles) '("bank" "Server notes")))
    (should (equal (lispm-1password--filter-label) "Work"))
    (lispm-1password-show-kind "SECURE_NOTE")
    (should (equal (lispm-1password-test--titles) '("Server notes")))
    (should (equal (lispm-1password--filter-label) "Work · Secure Note"))
    (with-current-buffer (lispm-1password--list-buffer)
      (should (equal (car (lispm-1password--status)) "1 of 4 items")))
    (lispm-1password-show-all)
    (lispm-1password-toggle-favorites)
    (should (equal (lispm-1password-test--titles) '("GitHub")))
    (lispm-1password-toggle-favorites)
    ;; Words match titles, details, vaults, websites and tags.
    (setq lispm-1password--search "work ADA")
    (should (equal (lispm-1password-test--titles) '("bank" "GitHub")))
    (setq lispm-1password--search "ops")
    (should (equal (lispm-1password-test--titles) '("Server notes")))
    (should (equal (lispm-1password--filter-label) "“ops”"))
    (lispm-1password-show-all)
    (should-not (lispm-1password-filtered-p))
    (should (equal (lispm-1password-category-name "WIRELESS_ROUTER") "Wireless Router"))
    (should (equal (lispm-1password-category-name "SOMETHING_NEW") "Something New"))
    (should (equal (lispm-1password--date "2026-09-01T08:30:00.123Z")
                   (format-time-string "%b %-d, %Y" (encode-time (iso8601-parse "2026-09-01T08:30:00.123Z")))))
    (should (equal (lispm-1password--date nil) ""))))

(ert-deftest lispm-1password-test-item-window ()
  "An item's window shows its fields, secrets concealed until revealed."
  (lispm-1password-test--with-op nil
    (lispm-1password-test--open)
    (save-window-excursion
      (lispm-1password-show (lispm-1password-find "ITEM1"))
      (lispm-1password-test--wait (lambda () (plist-get (lispm-1password-find "ITEM1") :whole)))
      (with-current-buffer lispm-1password-item-buffer-name
        (lispm-1password-test--wait (lambda () (string-search "recovery code" (buffer-string))))
        (let ((text (buffer-string)))
          (should (string-prefix-p "GitHub\nLogin · Personal · ★ Favorite" text))
          (should (string-search "Very Good" text))
          (should (string-search "https://github.com/login" text))
          (should (string-search "Two keys, one in the safe." text))
          (should (string-search "code, work/dev" text))
          (should-not (string-search "unused" text))
          (should-not (string-search "hunter2" text)))
        (goto-char (point-min))
        (lispm-1password-next-field)
        (should (equal (plist-get (lispm-1password--field-at-point) :id) "username"))
        (lispm-1password-next-field)
        (should (equal (plist-get (lispm-1password--field-at-point) :id) "password"))
        (lispm-1password-reveal)
        (lispm-1password-test--wait (lambda () (string-search "hunter2" (buffer-string))))
        (should (equal (lispm-1password--item-status) '("Personal" "Secret shown")))
        (should (lispm-1password--revealed-p))
        (lispm-1password-conceal)
        (should-not (string-search "hunter2" (buffer-string)))
        (should-not lispm-1password--revealed)
        (lispm-1password-next-field)
        (should (equal (plist-get (get-text-property (point) 'lispm-1password-field) :url)
                       "https://github.com/login"))
        (lispm-1password-next-field)
        (should (equal (plist-get (lispm-1password--field-at-point) :id) "TOTP1"))
        (lispm-1password-reveal)
        (lispm-1password-test--wait (lambda () (string-search "123 456" (buffer-string))))
        (lispm-1password-back)))
    ;; Archived, the item's window goes back to the list.
    (save-window-excursion
      (lispm-1password-show (lispm-1password-find "ITEM1"))
      (let ((window (selected-window)) (done nil))
        (lispm-1password-archive-item (lispm-1password-find "ITEM1") (lambda () (setq done t)))
        (lispm-1password-test--wait (lambda () done))
        (should (equal (buffer-name (window-buffer window)) lispm-1password-buffer-name))))))

(ert-deftest lispm-1password-test-page-while-locked ()
  "Locked, the list is empty and the page is shown; read, the list."
  (lispm-1password-test--with-op nil
    (should (lispm-1password--page-p))
    (should (string-prefix-p "Opening your vaults" (lispm-1password--page-text)))
    (lispm-1password-test--open)
    (should-not (lispm-1password--page-p))
    (lispm-1password-lock)
    (should (lispm-1password--page-p))
    ;; Without LispM Desktop, the buffer says it, with its buttons.
    (with-current-buffer (lispm-1password--list-buffer)
      (should-not (text-property-not-all (point-min) (point-max) 'tabulated-list-id nil))
      (should (string-search "1Password is locked." (buffer-string)))
      (goto-char (point-min))
      (should (equal (button-label (next-button (point))) "Unlock")))))

;;;; In LispM Desktop

(ert-deftest lispm-1password-test-desktop ()
  "In LispM Desktop, both windows have the application's interface."
  (skip-unless (locate-library "lispm-desktop-app-ui"))
  (require 'lispm-desktop-app-ui)
  (dolist (mode '(lispm-1password-mode lispm-1password-item-mode))
    (should (equal (plist-get (alist-get mode lispm-desktop-app-ui-skins) :title) "1Password")))
  (let ((list (alist-get 'lispm-1password-mode lispm-desktop-app-ui-skins)))
    (should (equal (plist-get list :empty)
                   '(lispm-1password--page-p lispm-1password--page-text lispm-1password--page-buttons)))
    (dolist (button (plist-get list :buttons))
      (should (commandp (nth 1 button))))))

(provide 'lispm-1password-test)
;;; lispm-1password-test.el ends here
