;;; lispm-1password-ui.el --- 1Password's window  -*- lexical-binding: t; -*-

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

;; M-x lispm-1password opens 1Password's window: the items of the
;; vaults, one a line, by title.  Its keys:
;;
;;   RET  open the item: its fields, websites, notes and tags
;;   s /  search, the list narrowing as words are typed
;;   v k  show one vault, one kind of item
;;   *    show the favorites only, or all
;;   a    show all the items again
;;   u    copy the username
;;   c    copy the password
;;   t    copy the one-time code
;;   w    open the website
;;   o    open the item in the 1Password app
;;   f    make the item a favorite, or no longer
;;   d    move the item to the Archive
;;   +    a new login, its password generated
;;   g    read the vaults again
;;   L    lock 1Password
;;
;; An item's window has the same keys, and RET there copies the field
;; on its line, r shows its secret, and q goes back to the list.
;;
;; In LispM Desktop, the App Store's 1Password opens it in a window of
;; its own, with buttons for its main commands, and a page saying what
;; to do while op is missing or 1Password is locked.  The page follows
;; what op answers from LispM Desktop 0.4.1, which shows it anew when
;; the buffer is written outside a command.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'browse-url)
(require 'iso8601)
(require 'lispm-1password)

(declare-function lispm-desktop-app-ui-define "lispm-desktop-app-ui" (key &rest properties))

(defface lispm-1password-title '((t :inherit bold :height 1.4))
  "Face of an item's title, atop its window."
  :group 'lispm-1password)

(defface lispm-1password-label '((t :inherit shadow))
  "Face of the labels of an item's fields."
  :group 'lispm-1password)

(defface lispm-1password-heading '((t :inherit bold))
  "Face of the headings of an item's sections."
  :group 'lispm-1password)

(defface lispm-1password-secret '((t :inherit fixed-pitch))
  "Face of a secret shown."
  :group 'lispm-1password)

(defconst lispm-1password-buffer-name "*1Password*"
  "Name of the buffer listing 1Password's items.")

(defconst lispm-1password-item-buffer-name "*1Password Item*"
  "Name of the buffer showing one of 1Password's items.")

(defconst lispm-1password-categories
  '(("LOGIN" . "Login") ("PASSWORD" . "Password") ("SECURE_NOTE" . "Secure Note")
    ("CREDIT_CARD" . "Credit Card") ("IDENTITY" . "Identity") ("DOCUMENT" . "Document")
    ("API_CREDENTIAL" . "API Credential") ("SSH_KEY" . "SSH Key") ("DATABASE" . "Database")
    ("SERVER" . "Server") ("SOFTWARE_LICENSE" . "Software License") ("BANK_ACCOUNT" . "Bank Account")
    ("WIRELESS_ROUTER" . "Wireless Router") ("EMAIL_ACCOUNT" . "Email Account")
    ("MEMBERSHIP" . "Membership") ("PASSPORT" . "Passport") ("DRIVER_LICENSE" . "Driver License")
    ("OUTDOOR_LICENSE" . "Outdoor License") ("REWARD_PROGRAM" . "Reward Program")
    ("SOCIAL_SECURITY_NUMBER" . "Social Security Number") ("MEDICAL_RECORD" . "Medical Record")
    ("CRYPTO_WALLET" . "Crypto Wallet"))
  "The kinds of items, as op names them, and as 1Password does.")

(defun lispm-1password-category-name (category)
  "Return the name 1Password gives the kind of item op calls CATEGORY."
  (or (cdr (assoc category lispm-1password-categories))
      (capitalize (string-replace "_" " " category))))

(defun lispm-1password--date (time)
  "Return TIME, as op writes it, as a date, or the empty string."
  (or (ignore-errors (format-time-string "%b %-d, %Y" (encode-time (iso8601-parse time))))
      ""))

;;;; What the list shows

(defvar lispm-1password--vault nil
  "The ID of the vault the list shows, or nil for all of them.")

(defvar lispm-1password--category nil
  "The kind of item the list shows, as op names it, or nil for all.")

(defvar lispm-1password--favorites nil
  "Non-nil while the list shows the favorites only.")

(defvar lispm-1password--search ""
  "The words the items the list shows all match.")

(defvar-local lispm-1password--shown nil
  "The ID of the item the buffer of `lispm-1password-item-mode' shows.")

(defvar-local lispm-1password--reading nil
  "Non-nil while the item the buffer shows is being read.")

(defvar-local lispm-1password--revealed nil
  "The secrets the item's window shows: (FIELD-ID . SECRET).")

(defvar-local lispm-1password--conceal-timer nil
  "The timer concealing the secrets the item's window shows.")

(defun lispm-1password--matches-p (item)
  "Return non-nil when ITEM matches every word searched for."
  (let ((text (downcase (string-join (append (list (plist-get item :title) (plist-get item :details)
                                                   (plist-get item :vault))
                                             (plist-get item :urls) (plist-get item :tags))
                                     " "))))
    (seq-every-p (lambda (word) (string-search word text))
                 (split-string (downcase lispm-1password--search) nil t))))

(defun lispm-1password--shown-p (item)
  "Return non-nil when the list shows ITEM."
  (and (or (null lispm-1password--vault) (equal (plist-get item :vault-id) lispm-1password--vault))
       (or (null lispm-1password--category) (equal (plist-get item :category) lispm-1password--category))
       (or (not lispm-1password--favorites) (plist-get item :favorite))
       (lispm-1password--matches-p item)))

(defun lispm-1password-filtered-p ()
  "Return non-nil when the list shows some of the items only."
  (or lispm-1password--vault lispm-1password--category lispm-1password--favorites
      (not (string-blank-p lispm-1password--search))))

(defun lispm-1password--vault-name (id)
  "Return the name of the vault of ID."
  (or (plist-get (seq-find (lambda (vault) (equal (plist-get vault :id) id)) lispm-1password-vaults) :name)
      "?"))

(defun lispm-1password--filter-label ()
  "Return what the list shows when it shows some of the items only, or nil."
  (when (lispm-1password-filtered-p)
    (string-join (delq nil (list (and lispm-1password--vault (lispm-1password--vault-name lispm-1password--vault))
                                 (and lispm-1password--category
                                      (lispm-1password-category-name lispm-1password--category))
                                 (and lispm-1password--favorites "Favorites")
                                 (unless (string-blank-p lispm-1password--search)
                                   (format "“%s”" (string-trim lispm-1password--search)))))
                 " · ")))

;;;; The list

(defun lispm-1password--entries ()
  "Return the lines of the list."
  (mapcar (lambda (item)
            (list (plist-get item :id)
                  (vector (if (plist-get item :favorite) "★" "")
                          (plist-get item :title)
                          (plist-get item :details)
                          (plist-get item :vault)
                          (lispm-1password-category-name (plist-get item :category))
                          (propertize (lispm-1password--date (plist-get item :updated))
                                      'lispm-1password-time (or (plist-get item :updated) "")))))
          (seq-filter #'lispm-1password--shown-p lispm-1password-items)))

(defun lispm-1password--title< (a b)
  "Non-nil if the line A comes before the line B by title, in any case."
  (string< (downcase (aref (cadr a) 1)) (downcase (aref (cadr b) 1))))

(defun lispm-1password--modified< (a b)
  "Non-nil if the line A was modified before the line B."
  (string< (get-text-property 0 'lispm-1password-time (aref (cadr a) 5))
           (get-text-property 0 'lispm-1password-time (aref (cadr b) 5))))

(defun lispm-1password--favorite< (a b)
  "Non-nil if the line A is of a favorite, and the line B is not."
  (and (string-empty-p (aref (cadr b) 0)) (not (string-empty-p (aref (cadr a) 0)))))

(defun lispm-1password--list-buffer ()
  "Return the buffer listing the items, made if need be."
  (or (get-buffer lispm-1password-buffer-name)
      (with-current-buffer (get-buffer-create lispm-1password-buffer-name)
        (lispm-1password-mode)
        (lispm-1password--print)
        (current-buffer))))

(defun lispm-1password--refresh ()
  "Show what 1Password knows now in its windows."
  (when-let* ((buffer (get-buffer lispm-1password-buffer-name)))
    (with-current-buffer buffer
      (lispm-1password--print)))
  (when-let* ((buffer (get-buffer lispm-1password-item-buffer-name)))
    (with-current-buffer buffer
      (if-let* ((item (lispm-1password-find lispm-1password--shown)))
          (progn (lispm-1password--render)
                 ;; Changed since it was read, as the vaults were read again.
                 (unless (or (plist-get item :whole) lispm-1password--reading)
                   (lispm-1password--read-shown)))
        ;; Archived, or 1Password locked: back to the list.
        (lispm-1password-conceal)
        (let ((list (lispm-1password--list-buffer)))
          (dolist (window (get-buffer-window-list buffer nil t))
            (set-window-buffer window list))))))
  ;; Changed outside a command, the buttons and the bottom are drawn anew.
  (force-mode-line-update t))

(defun lispm-1password--print ()
  "Write the list's buffer anew: the items, or, while there are none to
list, what the page says, and its buttons.  In LispM Desktop, the page
is drawn over the buffer, and drawn anew as the buffer is written: the
column names, aligned with display properties, would show it twice."
  (if (not (lispm-1password--page-p))
      (progn (tabulated-list-print t)
             (lispm-1password--onto-an-item))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "\n" (lispm-1password--page-text) "\n\n")
      (pcase-dolist (`(,label ,command) (lispm-1password--page-buttons))
        (insert-text-button label 'action (lambda (_) (call-interactively command)) 'follow-link t)
        (insert "  "))
      (insert "\n")
      (goto-char (point-min)))))

(defun lispm-1password--onto-an-item ()
  "Put point on the first item where it is on none, as on the column
names, in the list's buffer and its windows: the buttons act on the item
at point."
  (when-let* ((first (text-property-not-all (point-min) (point-max) 'tabulated-list-id nil)))
    (unless (tabulated-list-get-id)
      (goto-char first))
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (unless (get-text-property (window-point window) 'tabulated-list-id)
        (set-window-point window first)))))

(add-hook 'lispm-1password-changed-hook #'lispm-1password--refresh)

(defun lispm-1password--status ()
  "What the list's window says at its bottom."
  (when (eq lispm-1password-state 'ready)
    (let ((shown (seq-count #'lispm-1password--shown-p lispm-1password-items))
          (all (length lispm-1password-items)))
      (list (if (= shown all)
                (format "%d %s" all (if (= all 1) "item" "items"))
              (format "%d of %d items" shown all))
            (lispm-1password-account-name)))))

;;;; The page while there is no list

(defun lispm-1password--page-p ()
  "Return non-nil while 1Password has no items to list."
  (not (and lispm-1password-items (memq lispm-1password-state '(ready loading)))))

(defun lispm-1password--wrap (text)
  "Return TEXT broken into lines short enough for the page."
  (with-temp-buffer
    (insert text)
    (let ((fill-column 60))
      (fill-region (point-min) (point-max)))
    (buffer-string)))

(defun lispm-1password--page-text ()
  "Return what the page says, by where 1Password stands."
  (pcase lispm-1password-state
    ('missing
     (concat "1Password's command-line tool, op, is not installed.\n"
             (if lispm-1password-install-command
                 (format "Install it with  %s\nthen try again." lispm-1password-install-command)
               "Install it, then try again.")))
    ('no-account
     (concat "op knows no 1Password account yet.\n"
             "In the 1Password app, open Settings, then Developer,\n"
             "and turn on Integrate with 1Password CLI; then try again."))
    ('locked
     (concat "1Password is locked."
             (when lispm-1password-message
               (concat "\n" (lispm-1password--wrap lispm-1password-message)))))
    ('unlocking "Unlocking 1Password…\nIf 1Password asks, unlock it there.")
    ('error
     (concat "1Password could not be opened.\n" (lispm-1password--wrap (or lispm-1password-message ""))))
    ('ready "No items yet.")
    (_ "Opening your vaults…\nIf 1Password asks, unlock it there.")))

(defun lispm-1password--page-buttons ()
  "Return the page's buttons, by where 1Password stands."
  (pcase lispm-1password-state
    ('missing `(,@(when lispm-1password-install-command
                    '(("Copy Command" lispm-1password-copy-install-command)))
                ("Install Guide" lispm-1password-install-guide)
                ("Try Again" lispm-1password-reload)))
    ('no-account '(("Open 1Password" lispm-1password-open-app) ("Try Again" lispm-1password-reload)))
    ('locked '(("Unlock" lispm-1password-unlock)))
    ('error '(("Try Again" lispm-1password-reload)))
    ('ready '(("New Login…" lispm-1password-new)))))

(defun lispm-1password-copy-install-command ()
  "Copy the command installing op."
  (interactive)
  (lispm-1password-copy lispm-1password-install-command "the command installing op"))

(defun lispm-1password-install-guide ()
  "Open 1Password's page on installing op."
  (interactive)
  (browse-url lispm-1password-install-page))

(defun lispm-1password-open-app ()
  "Open the 1Password app."
  (interactive)
  (cond ((eq system-type 'darwin) (call-process "open" nil 0 nil "-a" "1Password"))
        ((executable-find "1password") (call-process "1password" nil 0 nil))
        (t (user-error "The 1Password app is not installed"))))

;;;; Items: which, and what to do with them

(defun lispm-1password--current-item ()
  "Return the item of the line at point, or the one the buffer shows; or
signal an error."
  (or (lispm-1password-find (if (derived-mode-p 'lispm-1password-item-mode)
                                lispm-1password--shown
                              (tabulated-list-get-id)))
      (user-error "No item here")))

(defun lispm-1password--item-p ()
  "Return non-nil when there is an item at point, or shown."
  (ignore-errors (lispm-1password--current-item)))

(defun lispm-1password--whole-p (test)
  "Return non-nil when the item shown, read whole, has a field TEST finds;
and when the item at point is not read yet."
  (when-let* ((item (lispm-1password--item-p)))
    (or (not (plist-get item :whole)) (funcall test item))))

(defun lispm-1password-open-item ()
  "Open the item at point: its fields, websites, notes and tags."
  (interactive)
  (lispm-1password-show (lispm-1password--current-item)))

(defun lispm-1password--copy (find what)
  "Copy the field FIND returns of the current item, read whole; WHAT says
which, when it has none."
  (lispm-1password-whole (lispm-1password--current-item)
                         (lambda (item)
                           (if-let* ((field (funcall find item)))
                               (lispm-1password-copy-field item field)
                             (message "%s has no %s" (plist-get item :title) what)))))

(defun lispm-1password-copy-username ()
  "Copy the item's username."
  (interactive)
  (lispm-1password--copy #'lispm-1password-username-field "username"))

(defun lispm-1password-copy-password ()
  "Copy the item's password; it leaves the clipboard again, see
`lispm-1password-clear-seconds'."
  (interactive)
  (lispm-1password--copy #'lispm-1password-password-field "password"))

(defun lispm-1password-copy-code ()
  "Copy the item's one-time code, as it is now."
  (interactive)
  (lispm-1password--copy #'lispm-1password-code-field "one-time password"))

(defun lispm-1password-open-website ()
  "Open the item's website, or the one on the line at point."
  (interactive)
  (let ((url (or (plist-get (get-text-property (point) 'lispm-1password-field) :url)
                 (car (plist-get (lispm-1password--current-item) :urls))
                 (user-error "This item has no website"))))
    (browse-url url)))

(defun lispm-1password-open-in-app ()
  "Open the item in the 1Password app."
  (interactive)
  (browse-url-default-browser (lispm-1password-link (lispm-1password--current-item))))

(defun lispm-1password-toggle-favorite ()
  "Make the item a favorite, or no longer."
  (interactive)
  (let ((item (lispm-1password--current-item)))
    (lispm-1password-set-favorite item (not (plist-get item :favorite)))))

(defun lispm-1password--favorite-p ()
  "Return non-nil when the current item is a favorite."
  (plist-get (lispm-1password--item-p) :favorite))

(defun lispm-1password-archive ()
  "Move the item to the Archive, once agreed to."
  (interactive)
  (let ((item (lispm-1password--current-item)))
    (when (y-or-n-p (format "Move %s to the Archive? " (plist-get item :title)))
      (lispm-1password-archive-item item (lambda ()
                                           (message "Moved %s to the Archive" (plist-get item :title)))))))

(defun lispm-1password--read-vault (prompt &optional all)
  "Read the name of a vault with PROMPT; return its ID.
With ALL, All Vaults may be chosen too, returning nil."
  (let* ((vaults (mapcar (lambda (vault) (cons (plist-get vault :name) (plist-get vault :id)))
                         lispm-1password-vaults))
         (choices (if all (cons (cons "All Vaults" nil) vaults) vaults)))
    (unless choices
      (user-error "No vault to choose"))
    (cdr (assoc (completing-read prompt choices nil t nil nil (caar choices)) choices))))

(defun lispm-1password--read-login ()
  "Read a new login: its title, vault, username, website and password."
  (unless (eq lispm-1password-state 'ready)
    (user-error "1Password is not open"))
  (let ((title (string-trim (read-string "Title of the login: "))))
    (when (string-empty-p title)
      (user-error "A login needs a title"))
    (let* ((vault (lispm-1password--read-vault "In the vault: "))
           (username (read-string "Username: "))
           (url (string-trim (read-string "Website: ")))
           (password (read-passwd "Password, or nothing to have one generated: " t)))
      (list title vault username url (if (string-empty-p password) nil password)))))

(defun lispm-1password-new (title vault username url password)
  "Create a login TITLE in VAULT, for USERNAME at URL, and open it.
Its password is PASSWORD; with PASSWORD nil, op generates one, which is
copied, ready to paste where it is set."
  (interactive (lispm-1password--read-login))
  (let ((window (selected-window)))
    (lispm-1password-create-login
     title vault username url password
     (lambda (item)
       (when (window-live-p window)
         (with-selected-window window
           (lispm-1password-show item)))
       (unless password
         (when-let* ((field (lispm-1password-password-field item)))
           (lispm-1password-copy-field item field)))))
    (when password
      (clear-string password))))

;;;; Choosing what the list shows

(defun lispm-1password-search ()
  "Show the items matching the words typed, narrowing the list as they are
typed; nothing typed shows them all."
  (interactive)
  (let ((before lispm-1password--search))
    (condition-case nil
        (minibuffer-with-setup-hook
            (lambda ()
              (add-hook 'after-change-functions
                        (lambda (&rest _)
                          (setq lispm-1password--search (minibuffer-contents-no-properties))
                          (lispm-1password--refresh))
                        nil t))
          (setq lispm-1password--search (read-from-minibuffer "Search 1Password: " before)))
      (quit (setq lispm-1password--search before)))
    (lispm-1password--refresh)
    (when (derived-mode-p 'lispm-1password-mode)
      (goto-char (point-min)))))

(defun lispm-1password-show-vault (vault)
  "Show the items of VAULT, an ID, only; or with VAULT nil, of all of them."
  (interactive (list (lispm-1password--read-vault "Show the vault: " t)))
  (setq lispm-1password--vault vault)
  (lispm-1password--refresh))

(defun lispm-1password-show-kind (category)
  "Show the items of the kind op calls CATEGORY only; or with nil, all."
  (interactive
   (let* ((kinds (mapcar (lambda (category) (cons (lispm-1password-category-name category) category))
                         (seq-uniq (mapcar (lambda (item) (plist-get item :category)) lispm-1password-items))))
          (choices (cons (cons "All Kinds" nil) (seq-sort-by #'car #'string< kinds))))
     (list (cdr (assoc (completing-read "Show the items of the kind: " choices nil t nil nil "All Kinds")
                       choices)))))
  (setq lispm-1password--category category)
  (lispm-1password--refresh))

(defun lispm-1password-toggle-favorites ()
  "Show the favorites only, or all the items again."
  (interactive)
  (setq lispm-1password--favorites (not lispm-1password--favorites))
  (lispm-1password--refresh))

(defun lispm-1password-show-all ()
  "Show all the items again: of every vault and kind, favorite or not,
searched for or not."
  (interactive)
  (setq lispm-1password--vault nil
        lispm-1password--category nil
        lispm-1password--favorites nil
        lispm-1password--search "")
  (lispm-1password--refresh))

(defun lispm-1password--vault-items (_items)
  "Return the View menu's vaults."
  (cons ["All Vaults" (lispm-1password-show-vault nil) :style radio :selected (null lispm-1password--vault)]
        (mapcar (lambda (vault)
                  (let ((id (plist-get vault :id)))
                    (vector (plist-get vault :name) `(lispm-1password-show-vault ,id)
                            :style 'radio :selected `(equal lispm-1password--vault ,id))))
                lispm-1password-vaults)))

(defun lispm-1password--kind-items (_items)
  "Return the View menu's kinds of items."
  (cons ["All Kinds" (lispm-1password-show-kind nil) :style radio :selected (null lispm-1password--category)]
        (mapcar (lambda (category)
                  (vector (lispm-1password-category-name category) `(lispm-1password-show-kind ,category)
                          :style 'radio :selected `(equal lispm-1password--category ,category)))
                (seq-sort-by #'lispm-1password-category-name #'string<
                             (seq-uniq (mapcar (lambda (item) (plist-get item :category))
                                               lispm-1password-items))))))

;;;; The list's mode

(defun lispm-1password--ellipsis ()
  "Show lines cut at the window's edge ending in an ellipsis, not a dollar."
  (setq buffer-display-table (or (copy-sequence buffer-display-table) (make-display-table)))
  (set-display-table-slot buffer-display-table 'truncation ?…))

(defvar-keymap lispm-1password-mode-map
  :doc "Keys of 1Password's list."
  :parent tabulated-list-mode-map
  "RET" #'lispm-1password-open-item
  "s" #'lispm-1password-search
  "/" #'lispm-1password-search
  "v" #'lispm-1password-show-vault
  "k" #'lispm-1password-show-kind
  "*" #'lispm-1password-toggle-favorites
  "a" #'lispm-1password-show-all
  "u" #'lispm-1password-copy-username
  "c" #'lispm-1password-copy-password
  "t" #'lispm-1password-copy-code
  "w" #'lispm-1password-open-website
  "o" #'lispm-1password-open-in-app
  "f" #'lispm-1password-toggle-favorite
  "d" #'lispm-1password-archive
  "+" #'lispm-1password-new
  "g" #'lispm-1password-reload
  "L" #'lispm-1password-lock)

(defun lispm-1password--ready-p ()
  "Return non-nil once the vaults are read."
  (eq lispm-1password-state 'ready))

(easy-menu-define lispm-1password-menu lispm-1password-mode-map
  "1Password's menu."
  '("1Password"
    ["New Login…" lispm-1password-new :active (lispm-1password--ready-p)]
    ["Open" lispm-1password-open-item :active (lispm-1password--item-p)]
    "---"
    ["Copy Username" lispm-1password-copy-username :active (lispm-1password--item-p)]
    ["Copy Password" lispm-1password-copy-password :active (lispm-1password--item-p)]
    ["Copy One-Time Code" lispm-1password-copy-code :active (lispm-1password--item-p)]
    ["Open Website" lispm-1password-open-website
     :active (plist-get (lispm-1password--item-p) :urls)]
    ["Open in 1Password" lispm-1password-open-in-app :active (lispm-1password--item-p)]
    "---"
    ["Favorite" lispm-1password-toggle-favorite :style toggle :selected (lispm-1password--favorite-p)
     :active (lispm-1password--item-p)]
    ["Move to Archive…" lispm-1password-archive :active (lispm-1password--item-p)]
    "---"
    ["Read Again" lispm-1password-reload]
    ["Lock" lispm-1password-lock :active (lispm-1password--ready-p)]))

(easy-menu-define lispm-1password-view-menu lispm-1password-mode-map
  "1Password's View menu."
  '("View"
    ["All Items" lispm-1password-show-all :style radio :selected (not (lispm-1password-filtered-p))]
    ["Favorites" lispm-1password-toggle-favorites :style toggle :selected lispm-1password--favorites]
    ["Search…" lispm-1password-search :active (lispm-1password--ready-p)]
    "---"
    ("Vault" :filter lispm-1password--vault-items)
    ("Kind" :filter lispm-1password--kind-items)))

(define-derived-mode lispm-1password-mode tabulated-list-mode "1Password"
  "1Password's list: the items of the vaults, one a line, by title.
\\{lispm-1password-mode-map}"
  (setq tabulated-list-format [("★" 2 lispm-1password--favorite<)
                               ("Title" 24 lispm-1password--title<)
                               ("Details" 18 t)
                               ("Vault" 10 t)
                               ("Kind" 16 t)
                               ("Modified" 0 lispm-1password--modified<)]
        tabulated-list-padding 1
        tabulated-list-sort-key '("Title")
        tabulated-list-entries #'lispm-1password--entries)
  (lispm-1password--ellipsis)
  (tabulated-list-init-header))

;;;; An item's window

(defconst lispm-1password--dots "••••••••••"
  "What a secret concealed shows.")

(defun lispm-1password-conceal ()
  "Conceal the secrets the item's window shows, and forget them."
  (interactive)
  (when lispm-1password--conceal-timer
    (cancel-timer lispm-1password--conceal-timer)
    (setq lispm-1password--conceal-timer nil))
  (when lispm-1password--revealed
    (dolist (revealed lispm-1password--revealed)
      (clear-string (cdr revealed)))
    (setq lispm-1password--revealed nil)
    (when (derived-mode-p 'lispm-1password-item-mode)
      (lispm-1password--render))))

(defun lispm-1password--field-at-point ()
  "Return the field on the line at point, or nil."
  (let ((field (get-text-property (point) 'lispm-1password-field)))
    (and (plist-get field :id) field)))

(defun lispm-1password-reveal ()
  "Show the secret on the line at point, or the item's password; or, when
one is shown, conceal it.  It is concealed again after
`lispm-1password-reveal-seconds'."
  (interactive)
  (let* ((item (lispm-1password--current-item))
         (at-point (lispm-1password--field-at-point))
         (field (if (plist-get at-point :secret) at-point (lispm-1password-password-field item)))
         (buffer (current-buffer)))
    (cond ((null field) (user-error "Nothing secret to show here"))
          ((assoc (plist-get field :id) lispm-1password--revealed)
           (lispm-1password-conceal))
          (t
           (lispm-1password-read-secret
            item field
            (lambda (secret)
              (if (not (buffer-live-p buffer))
                  (clear-string secret)
                (with-current-buffer buffer
                  (push (cons (plist-get field :id) secret) lispm-1password--revealed)
                  (when lispm-1password-reveal-seconds
                    (when lispm-1password--conceal-timer
                      (cancel-timer lispm-1password--conceal-timer))
                    (setq lispm-1password--conceal-timer
                          (run-at-time lispm-1password-reveal-seconds nil
                                       (lambda ()
                                         (when (buffer-live-p buffer)
                                           (with-current-buffer buffer
                                             (lispm-1password-conceal)))))))
                  (lispm-1password--render)))))))))

(defun lispm-1password--revealed-p ()
  "Return non-nil while the item's window shows a secret."
  (and (derived-mode-p 'lispm-1password-item-mode) lispm-1password--revealed t))

(defun lispm-1password-copy-at-point ()
  "Copy the field on the line at point, its secret read first; or open
the website on it."
  (interactive)
  (let ((field (or (get-text-property (point) 'lispm-1password-field)
                   (user-error "No field here"))))
    (if-let* ((url (plist-get field :url)))
        (browse-url url)
      (lispm-1password-copy-field (lispm-1password--current-item) field))))

(defun lispm-1password-next-field (&optional n)
  "Go to the next field, or the Nth; back with N negative."
  (interactive "p")
  (let ((n (or n 1)))
    (dotimes (_ (abs n))
      (let ((start (point))
            (found nil))
        (while (and (not found) (zerop (forward-line (if (< n 0) -1 1))) (not (eobp)))
          (setq found (get-text-property (point) 'lispm-1password-field)))
        (unless found
          (goto-char start))))))

(defun lispm-1password-previous-field (&optional n)
  "Go to the previous field, or the Nth back."
  (interactive "p")
  (lispm-1password-next-field (- (or n 1))))

(defun lispm-1password-back ()
  "Go back to the list."
  (interactive)
  (lispm-1password-conceal)
  (switch-to-buffer (lispm-1password--list-buffer)))

(defun lispm-1password--read-shown ()
  "Read the item the current buffer shows, whole."
  (let* ((buffer (current-buffer))
         (done (lambda (&rest _)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (setq lispm-1password--reading nil))))))
    (setq lispm-1password--reading t)
    (lispm-1password-read-item (lispm-1password--current-item) done
                               (lambda (said)
                                 (funcall done)
                                 (lispm-1password--action-failed said)))))

(defun lispm-1password-read-again ()
  "Read the item shown again."
  (interactive)
  (lispm-1password--read-shown))

(defvar-keymap lispm-1password--value-map
  :doc "Keys of the values of an item's fields: the mouse acts on them, and
the keyboard as on the rest of their line."
  "<mouse-1>" #'push-button
  "<mouse-2>" #'push-button)

(defun lispm-1password--insert-line (label value field &optional note)
  "Insert a line of LABEL and VALUE, copied as FIELD says, NOTE after it."
  (let ((start (point)))
    (insert (propertize label 'face 'lispm-1password-label)
            (propertize " " 'display '(space :align-to 22)))
    (insert-text-button value
                        'face (if (plist-get field :secret) 'lispm-1password-secret 'default)
                        'mouse-face 'highlight
                        'help-echo (if (plist-get field :url) "Open the website" "Copy")
                        'keymap lispm-1password--value-map
                        'action (let ((field field))
                                  (lambda (_)
                                    (if-let* ((url (plist-get field :url)))
                                        (browse-url url)
                                      (lispm-1password-copy-field (lispm-1password--current-item) field)))))
    (when note
      (insert "  " (propertize note 'face 'shadow)))
    (insert "\n")
    (put-text-property start (point) 'lispm-1password-field field)))

(defun lispm-1password--field-value (field)
  "Return what FIELD's line shows: its value, or its secret, revealed or not."
  (if (plist-get field :secret)
      (let ((secret (cdr (assoc (plist-get field :id) lispm-1password--revealed))))
        (cond ((and secret (equal (plist-get field :type) "OTP") (= (length secret) 6))
               (concat (substring secret 0 3) " " (substring secret 3)))
              (secret)
              ((equal (plist-get field :type) "OTP") "••• •••")
              (t lispm-1password--dots)))
    (plist-get field :value)))

(defun lispm-1password--shown-field-p (field)
  "Return non-nil when FIELD has a line of its own: it holds something,
and is not the notes."
  (and (not (plist-get field :empty))
       (not (equal (plist-get field :purpose) "NOTES"))
       (or (plist-get field :secret)
           (not (string-empty-p (or (plist-get field :value) ""))))))

(defun lispm-1password--insert-fields (fields)
  "Insert a line for each of FIELDS."
  (dolist (field fields)
    (when (lispm-1password--shown-field-p field)
      (lispm-1password--insert-line
       (plist-get field :label) (lispm-1password--field-value field) field
       (cond ((plist-get field :strength)
              (capitalize (string-replace "_" " " (plist-get field :strength))))
             ((and (equal (plist-get field :type) "OTP")
                   (not (assoc (plist-get field :id) lispm-1password--revealed)))
              "t copies the code"))))))

(defun lispm-1password--insert-item (item)
  "Insert what ITEM holds, as its window shows it."
  (insert (propertize (plist-get item :title) 'face 'lispm-1password-title) "\n")
  (insert (propertize (string-join (delq nil (list (lispm-1password-category-name (plist-get item :category))
                                                   (plist-get item :vault)
                                                   (and (plist-get item :favorite) "★ Favorite")))
                                   " · ")
                      'face 'shadow)
          "\n\n")
  (if (not (plist-get item :whole))
      (insert (propertize "Reading…" 'face 'shadow) "\n")
    (let ((fields (plist-get item :fields)))
      (lispm-1password--insert-fields (seq-remove (lambda (field) (plist-get field :section)) fields))
      (dolist (url (plist-get item :urls))
        (lispm-1password--insert-line "website" url (list :url url)))
      (dolist (section (plist-get item :sections))
        (let ((in (seq-filter (lambda (field)
                                (and (equal (plist-get field :section) (plist-get section :id))
                                     (lispm-1password--shown-field-p field)))
                              fields)))
          (when in
            (insert "\n")
            (when-let* ((label (plist-get section :label))
                        ((not (string-empty-p label))))
              (insert (propertize label 'face 'lispm-1password-heading) "\n"))
            (lispm-1password--insert-fields in))))
      (when-let* ((notes (lispm-1password-field item (lambda (field) (equal (plist-get field :purpose) "NOTES"))))
                  (text (plist-get notes :value))
                  ((not (string-empty-p text))))
        (insert "\n" (propertize "Notes" 'face 'lispm-1password-heading) "\n" text "\n"))
      (when-let* ((tags (plist-get item :tags)))
        (insert "\n" (propertize "Tags" 'face 'lispm-1password-label) "  " (string-join tags ", ") "\n"))
      (insert "\n" (propertize (format "Modified %s · Created %s"
                                       (lispm-1password--date (plist-get item :updated))
                                       (lispm-1password--date (plist-get item :created)))
                               'face 'shadow)
              "\n"))))

(defun lispm-1password--render ()
  "Show the item of the current buffer anew, keeping the line."
  (when-let* ((item (lispm-1password-find lispm-1password--shown)))
    (let ((inhibit-read-only t)
          (line (line-number-at-pos)))
      (erase-buffer)
      (lispm-1password--insert-item item)
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun lispm-1password-show (item)
  "Show ITEM in the selected window, read whole."
  (let ((buffer (get-buffer-create lispm-1password-item-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'lispm-1password-item-mode)
        (lispm-1password-item-mode))
      (unless (equal lispm-1password--shown (plist-get item :id))
        (lispm-1password-conceal)
        (setq lispm-1password--shown (plist-get item :id))
        (let ((inhibit-read-only t))
          (erase-buffer)))
      (lispm-1password--render)
      (goto-char (point-min))
      (lispm-1password--read-shown))
    (switch-to-buffer buffer)))

(defun lispm-1password--item-status ()
  "What an item's window says at its bottom."
  (when-let* ((item (lispm-1password-find lispm-1password--shown)))
    (list (plist-get item :vault)
          (and lispm-1password--revealed "Secret shown"))))

(defvar-keymap lispm-1password-item-mode-map
  :doc "Keys of the window of one of 1Password's items."
  :parent special-mode-map
  "RET" #'lispm-1password-copy-at-point
  "n" #'lispm-1password-next-field
  "p" #'lispm-1password-previous-field
  "r" #'lispm-1password-reveal
  "u" #'lispm-1password-copy-username
  "c" #'lispm-1password-copy-password
  "t" #'lispm-1password-copy-code
  "w" #'lispm-1password-open-website
  "o" #'lispm-1password-open-in-app
  "f" #'lispm-1password-toggle-favorite
  "d" #'lispm-1password-archive
  "g" #'lispm-1password-read-again
  "q" #'lispm-1password-back
  "l" #'lispm-1password-back)

(easy-menu-define lispm-1password-item-menu lispm-1password-item-mode-map
  "The menu of 1Password's item."
  '("Item"
    ["Copy Field" lispm-1password-copy-at-point :active (get-text-property (point) 'lispm-1password-field)]
    ["Copy Username" lispm-1password-copy-username]
    ["Copy Password" lispm-1password-copy-password]
    ["Copy One-Time Code" lispm-1password-copy-code]
    ["Show Secret" lispm-1password-reveal :style toggle :selected (lispm-1password--revealed-p)]
    "---"
    ["Open Website" lispm-1password-open-website :active (plist-get (lispm-1password--item-p) :urls)]
    ["Open in 1Password" lispm-1password-open-in-app]
    "---"
    ["Favorite" lispm-1password-toggle-favorite :style toggle :selected (lispm-1password--favorite-p)]
    ["Move to Archive…" lispm-1password-archive]
    "---"
    ["Back to the List" lispm-1password-back]))

(define-derived-mode lispm-1password-item-mode special-mode "1Password Item"
  "One of 1Password's items: its fields, websites, notes and tags.
\\{lispm-1password-item-mode-map}"
  (setq truncate-lines t)
  (add-hook 'kill-buffer-hook #'lispm-1password-conceal nil t)
  (add-hook 'change-major-mode-hook #'lispm-1password-conceal nil t))

;;;###autoload
(defun lispm-1password ()
  "Open 1Password's window: the items of the vaults, one a line.
The vaults are read first, and 1Password unlocked if need be."
  (interactive)
  (switch-to-buffer (lispm-1password--list-buffer))
  (unless (memq lispm-1password-state '(ready loading unlocking))
    (lispm-1password-reload))
  (lispm-1password--refresh))

;;;; In LispM Desktop

(defun lispm-1password--desktop-setup ()
  "Name the list's columns in the buffer: in LispM Desktop, the strip of
buttons takes the window's header line."
  (setq-local tabulated-list-use-header-line nil)
  (tabulated-list-init-header)
  (lispm-1password--print))

(with-eval-after-load 'lispm-desktop-app-ui
  (lispm-desktop-app-ui-define
   'lispm-1password-mode
   :title "1Password"
   :list t
   :setup #'lispm-1password--desktop-setup
   :buttons `(("New…" lispm-1password-new lispm-1password--ready-p)
              ("Open" lispm-1password-open-item lispm-1password--item-p)
              ("Copy Password" lispm-1password-copy-password lispm-1password--item-p)
              ("Copy Code" lispm-1password-copy-code lispm-1password--item-p)
              ("Search…" lispm-1password-search lispm-1password--ready-p)
              ("Show All" lispm-1password-show-all lispm-1password-filtered-p))
   :label #'lispm-1password--filter-label
   :status #'lispm-1password--status
   :empty '(lispm-1password--page-p lispm-1password--page-text lispm-1password--page-buttons))
  (lispm-desktop-app-ui-define
   'lispm-1password-item-mode
   :title "1Password"
   :prose t
   :buttons `(("Back" lispm-1password-back)
              ("Copy Username" lispm-1password-copy-username
               ,(lambda () (lispm-1password--whole-p #'lispm-1password-username-field)))
              ("Copy Password" lispm-1password-copy-password
               ,(lambda () (lispm-1password--whole-p #'lispm-1password-password-field)))
              ("Copy Code" lispm-1password-copy-code
               ,(lambda () (lispm-1password--whole-p #'lispm-1password-code-field)))
              (,(lambda () (if lispm-1password--revealed "Conceal" "Reveal")) lispm-1password-reveal
               ,(lambda () (lispm-1password--whole-p #'lispm-1password-password-field)))
              ("Website" lispm-1password-open-website
               ,(lambda () (plist-get (lispm-1password--item-p) :urls))))
   :status #'lispm-1password--item-status))

(provide 'lispm-1password-ui)
;;; lispm-1password-ui.el ends here
