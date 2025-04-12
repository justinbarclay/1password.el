;;; 1password.el --- Emacs ❤️ 1Password -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Justin Barclay

;; Author: Justin Barclay <emacs@justinbarclay.ca>
;; URL: https://github.com/justinbarclay/1password.el
;; Version: 0.1.1
;; Package-Requires: ((aio "1.0") (emacs "28.2"))
;; Keywords: processes, convenience

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Emacs integration with the 1Password CLI.

;;; Code:
;;

(require '1password-lib)
(require '1password-item)
(require 'aio)
(require 'auth-source)
(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'tabulated-list)
(require 'widget)

(defgroup 1password nil
  "1Password integration for Emacs."
  :group 'applications)

(defcustom 1password-executable (if (eq system-type 'windows-nt)
                                    "op.exe"
                                  "op")
  "The fully qualified path to the 1Password executable."
  :type '(string)
  :options '("op" "op.exe")
  :group '1password)

(defcustom 1password-results-formatter '1password-default-formatter
  "The function used to format the results for minibuffer completion."
  :type '(function)
  :group '1password)

(defgroup 1Password-faces nil
  "Faces used by `1Password'."
  :group '1Password
  :group 'faces)

(defface 1password--title-face '((t :inherit default))
  "Face to use for the title of the 1Password entry.")

(defface 1password--additional-information-face '((t :inherit font-lock-warning-face))
  "Face to use for the additional information of the 1Password entry.")

(defface 1password--vault-name-face '((t :inherit font-lock-comment-face))
  "Face for the vault name when for the 1Password entry.")


(defvar 1password--item-cache nil "A cache for 1Password's `item list' command.")

(defvar 1password--template-file "op-template" "The name of the template file used to create new items.")

(defvar 1password--categories '("API Credential"
                                "Bank Account"
                                "Credit Card"
                                "Database"  "Document"
                                "Driver License"
                                "Email Account"
                                "Identity"
                                "Login"
                                "Membership"
                                "Outdoor License"
                                "Passport"
                                "Password"
                                "Reward Program"
                                "Secure Note"
                                "Server"
                                "Software License"
                                "Wireless Router"))

;; Retrieve a shareable link for the item referenced by ID:
;;
;;         op item get kiramv6tpjijkuci7fig4lndta --vault "Ops Secrets" --share-link
;;
;; Flags:
;;       --fields fields     Only return data from these fields. Use 'label=' to get the field by name or 'type=' to filter fields by type.
;;   -h, --help              Get help with item get.
;;       --include-archive   Include items in the Archive. Can also be set using OP_INCLUDE_ARCHIVE environment variable.
;;       --otp               Output the primary one-time password for this item.
;;       --share-link        Get a shareable link for the item.
;;       --vault vault       Look for the item in this vault.
;;
;; TODO Refactor
(cl-defun 1password--get-query-builder (&key id-or-name
                                             (field-keys '(username password))
                                             vault
                                             &allow-other-keys)
  "Builds a query for 1Password's `get item' command using `ID-OR-NAME'.

`FIELD-KEYS' is the list of fields that 1Password CLI will try
and return from the entry that matches `ID-OR-NAME'.

`VAULT' is the name of the vault to search for the entry in."
  (let ((fields (mapconcat #'symbol-name field-keys ",")))
    (string-join (append (list "item"
                               "get"
                               id-or-name
                               "--format" "json")
                         (when field-keys (list "--fields" fields))
                         (when vault (list "--vault" vault)))
                 " ")))

(defun 1password--find-vault (id)
  "Search `1password--item-cache' for the vault name of the entry with `ID'."
  (thread-last (cl-find id 1password--item-cache :test
                        (lambda (target item)
                          (eq (gethash "id" item) target)))
               (gethash "vault")
               (gethash "name")))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1Password Auth Source
;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defun 1password--search (&rest args &key id-or-name field-keys vault)
  "Search 1Password for an entry using information within in `ARGS'.

1Password--search will search in `VAULT' for an entry matching
`ID-OR-NAME'.  If no `VAULT' is specified it will search through
all vaults.

1Password--search will return at most a list with 1
item if a match is found.  If any matches are found
1Password--search will try to return the fields specified in
`FIELD-KEYS'."
  (condition-case json-error
      (thread-first (apply #'1password--get-query-builder args)
                    1password--execute-in-buffer
                    1password--extract-data)
    ;; If we get an error, we can try to parse the buffer for ids
    ;; and ask the user for the right entry
    (error (if-let* ((ids (1password--parse-buffer-for-ids))
                     (id (1password--search-id ids)))
               (thread-first (apply #'1password--get-query-builder :id-or-name id :field-keys field-keys :vault vault)
                             1password--execute-in-buffer
                             1password--extract-data)
             json-error))))

(defun 1password--parse-buffer-for-ids ()
  "Return a list of all ids found in the *1password* buffer.

When searching for entries in 1Password, if the query is not
specific enough it is possible for 1Password to find multiple
entries.  We can parse out these entries and try to give the user
a second chance.

For example if the *1password* buffer contains:

[ERROR] 2023/05/20 00:20:53 More than one item matches \"login\".
Try again and specify the item by its ID:
  * for the item \"Login\" in vault Private: ai46l2ccrszcvygm2v6lltgchq
  * for the item \"Login\" in vault Private: gijwlc4a2iipno6rm47sx6bxni
  * for the item \"Login\" in vault Private: j6f64bfgbg3wssv6lklmsc6ayu
  * for the item \"Login\" in vault Private: hpn47a3vncdtz77wg7eg6ix6ea

Then if we called:
\(1password--parse-buffer-for-ids\)

=>  (\"hpn47a3vncdtz77wg7eg6ix6ea\"
     \"j6f64bfgbg3wssv6lklmsc6ayu\"
     \"gijwlc4a2iipno6rm47sx6bxni\"
     \"ai46l2ccrszcvygm2v6lltgchq\")"
  (with-current-buffer (get-buffer-create "*1password*")
    (let ((ids '()))
      (goto-char (point-min))
      (while (not (eq (point-max)
                      (point)))
        (search-forward "Private: " nil 1)
        (and (not (eq (point)
                      (point-max)))
             (setq ids (cons (buffer-substring (point) (pos-eol)) ids))))
      ids)))

(cl-defun 1password--auth-source-search (&rest spec
                                         &key host id
                                         &allow-other-keys)
  "Execute 1Passwords `get item' command on the `HOST' or `ID' key.

If both `ID' and `HOST' are specified in `SPEC',
1password--auth-source-search will search by id.

If the host you are searching for has more than 1 entry within
1Password, `1password--auth-source-search' will throw a parsing
error.  If this is the case it is recommended that you set the
host to be a unique name or use the 1Password ID for the item.

You can use `1password-search-id' to find the id for of an entry."
  (thread-last (1password--search :id-or-name (or host id))
               (cl-substitute :secret :password)
               (cl-substitute :user :username)
               (append (list :backend '1password :host host :port nil))
               (list)))

(defvar 1password-auth-source-backend
  (auth-source-backend
   :source "."
   :type '1password
   :search-function #'1password--auth-source-search))

(defun 1password-auth-source-backend-parse (entry)
  "Parse `ENTRY' for 1Password specific parameters."
  (when (eq entry '1password)
    (auth-source-backend-parse-parameters entry 1password-auth-source-backend)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Candidate Formatting
;;;;;;;;;;;;;;;;;;;;;;;;;
(defun 1password-colour-formatter (results)
  "Format the cached `RESULTS' from 1Password for Minibuffer Completion."
  (let* ((fields '("title" "additional_information" ("vault" "name")))
         (spacings (1password--max-candidate-lengths results fields)))
    (mapcar (lambda (candidate)
              (list
               (string-join
                (cl-mapcar
                 (lambda (field)
                   (let* ((format-string (string-join
                                          (list "%-"
                                                (number-to-string (gethash field spacings 0))
                                                "s")))
                          (formatted-string (format format-string
                                                    (1password--nested-get field candidate ""))))
                     (propertize formatted-string
                                 'face (cond
                                        ((and (listp field)
                                              (equal field '("vault" "name")))
                                         '1password--vault-name-face)
                                        ((string= field "title") '1password--title-face)
                                        ((string= field "additional_information") '1password--additional-information-face)
                                        (t 'default)))))
                 fields)
                " ")
               (gethash "id" candidate)))
            results)))

(defun 1password-default-formatter (results)
  "Format the cached `RESULTS' from 1Password for Minibuffer Completion."
  (mapcar
   (lambda (response)
     (list
      (gethash "title" response)
      (gethash "id" response)))
   results))

(defun 1password-build-item-view (details item-id)
  (let* ((item-title (or (gethash "title" details) item-id))
         (details-buffer-name (format "*1Password Edit: %s*" item-title))
         (details-buffer (get-buffer-create details-buffer-name))
         ;; Store original values to detect changes
         (original-values (make-hash-table :test 'equal)))
    (with-current-buffer details-buffer
      (let ((inhibit-read-only t)
            (standard-output details-buffer)) ; Make widget output go here
        (erase-buffer)
        ;; (widgets-minor-mode)

        ;; --- Header Info (Read Only) ---
        (widget-create 'widget-read-only
                       :value (format "Editing Item: %s (ID: %s)" item-title item-id)
                       :format "%v\n\n")
        (widget-create 'widget-read-only
                       :value (format "Vault: %s" (gethash "name" (gethash "vault" details "")))
                       :format "  %v\n")
        (widget-create 'widget-read-only
                       :value (format "Category: %s" (gethash "category" details ""))
                       :format "  %v\n\n")

        ;; --- Editable Title ---
        (let ((orig-title (gethash "title" details "")))
          (puthash "title" orig-title original-values)
          (widget-create 'widget-editable-field
                         :format "Title:    %v\n"
                         :value orig-title
                         'widget-id "title")) ; Use widget-id for easy retrieval

        ;; --- Editable Username & Password (if found) ---
        (let ((fields (gethash "fields" details))) ; Get fields, could be nil or not a vector
          (when (vectorp fields) ; Only proceed if fields is actually a vector
            (dolist (field (coerce fields 'list))
              (let ((purpose (gethash "purpose" field))
                    (label (gethash "label" field))
                    (value (gethash "value" field ""))))
              ;; Username
              (when (string= purpose "USERNAME")
                (puthash "username" value original-values)
                (widget-create 'widget-editable-field
                               :format "Username: %v\n"
                               :value value
                               'widget-id "username"))
              ;; Password
              (when (string= purpose "PASSWORD")
                (puthash "password" value original-values)
                (widget-create 'widget-secret ; Use widget-secret for passwords
                               :format "Password: %v\n"
                               :value value
                               'widget-id "password"))))) ; Close the when and dolist

        ;; --- Separator & Buttons ---
        (widget-create 'widget-hrule :width 40 :format "\n%v\n\n")

        (widget-create
         'widget-button
         :notify (lambda (&rest _)
                   (let ((assignments '())
                         (widgets (widget-children)))
                     ;; Collect changed values
                     (dolist (w widgets)
                       (let ((id (widget-get w 'widget-id)))
                         (when id ; Only check widgets we tagged with an id
                           (let ((current-val (widget-value w))
                                 (original-val (gethash id original-values)))
                             (unless (equal current-val original-val)
                               (push (format "%s=%s" id current-val) assignments))))))

                     (if assignments
                         (progn
                           (message "Saving changes...")
                           (aio-call (1password--item-edit item-id (nreverse assignments))
                                     (lambda (result)
                                       (message "Item %s saved successfully." item-id)
                                       ;; Optionally refresh list or close buffer
                                       (kill-buffer (current-buffer))
                                       ;; Refresh the list buffer if it exists
                                       (when-let ((list-buf (get-buffer "*1Password Items*")))
                                         (with-current-buffer list-buf
                                           (revert-buffer t t t)))) ; Non-interactive revert
                                     (lambda (err)
                                       ;; Ensure err is a string for display
                                       (let ((err-msg (if (stringp err) err (format "%S" err))))
                                         (message "Error saving item %s: %s" item-id err-msg)
                                         (display-warning '1password (format "Save failed: %s" err-msg) :error)))))
                       (message "No changes to save.")
                       (kill-buffer (current-buffer))))) ; Close if no changes
         "Save Changes")

        (insert "  ") ; Spacer

        (widget-create
         'widget-button
         :notify (lambda (&rest _)
                   (kill-buffer (current-buffer))
                   (message "Edit cancelled."))
         "Cancel")

        ;; --- Final Setup ---
        (widget-setup)
        (goto-char (point-min))))))
(aio-defun 1password-show-item-details ()
  "Fetch and display details for the 1Password item on the current line using widgets."
  (interactive)
  (let ((item-id (tabulated-list-get-id)))
    (when item-id
      (message "Fetching details for %s..." item-id)
      (let ((details (aio-await (1password--item-get item-id)))) ; details should be a parsed Lisp object (hash-table)
        (message "%s" details)
        ;; Ensure details is a hash-table before proceeding
        (unless (hash-table-p details)
          (error "Failed to parse item details for %s. Expected hash-table, got: %S" item-id details))
        (1password-build-item-view details item-id)

        (pop-to-buffer details-buffer)
        (message "Displaying details for %s in editable form." item-title)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Commands
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload (autoload '1password-list "1password" nil t)
(aio-defun 1password-list ()
  "List all 1Password items using tabulated-list-mode."
  (interactive)
  (let* ((items (aio-await (1password--item-list))) ; Use non-cached version for freshness
         (buffer-name "*1Password Items*")
         ;; Define columns: Name, Width, Sortable (nil means not sortable by clicking header)
         (tabulated-list-format
          [("Title"    30 nil)
           ("Vault"    20 nil)
           ("Category" 15 nil)])
         ;; Prepare entries: (id [column-vector])
         (tabulated-list-entries
          (mapcar (lambda (item)
                    (let ((id (gethash "id" item))
                          (title (gethash "title" item ""))
                          ;; Safely get vault name
                          (vault-name (let ((vault-hash (gethash "vault" item)))
                                        (if vault-hash (gethash "name" vault-hash "") "")))
                          (category (gethash "category" item "")))
                      (list id (vector title vault-name category))))
                  items)))
    ;; Create and populate the buffer
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (tabulated-list-mode)
        ;; Bind RET (Enter) to show details
        (define-key tabulated-list-mode-map (kbd "RET") #'1password-show-item-details)
        ;; tabulated-list-entries and tabulated-list-format are buffer-local
        ;; and set via the outer let*. Now initialize and print.
        (tabulated-list-init-header)
        (tabulated-list-print)))
    (pop-to-buffer buffer-name)))
;;;###autoload (autoload '1password-enable-auth-source "1password" nil t)
(defun 1password-enable-auth-source ()
  "Enable 1Password integration with auth-source."
  (interactive)
  (add-hook 'auth-source-backend-parser-functions #'1password-auth-source-backend-parse)
  (add-to-list 'auth-sources '1password))

;;;###autoload (autoload '1password-disable-auth-source "1password" nil t)
(defun 1password-disable-auth-source ()
  "Remove 1Password from auth-source integration."
  (interactive)
  (remove-hook 'auth-source-backend-parser-functions #'1password-auth-source-backend-parse)
  (setq auth-sources (remove '1password auth-sources)))

;; TODO: Add support for custom categories
;; TODO: Add support for more than 1 emails
;;;###autoload (autoload '1password-share "1password" nil t)
(aio-defun 1password-share ()
  "Shares the selected 1Password entry to the specified entry."
  (interactive)

  (let* ((id (aio-await (1password--search-id)))
         (response (aio-await
                    (1password--share id
                                      (read-string "Email: ")))))
    (if response
        (progn
          (kill-new response)
          (message "1Password share link copied to clipboard"))
      (message "1Password entry not found or does not contain a share link"))))


;;;###autoload (autoload '1password-search-id "1password" nil t)
(aio-defun 1password-search-id ()
  "Search for 1Password id by entry name by title."
  (interactive)
  (kill-new (aio-await (1password--search-id)))
  (message "1Password ID copied to clipboard"))

;;;###autoload (autoload '1password-search-password "1password" nil t)
(aio-defun 1password-search-password ()
  "Search for password by entry name."
  (interactive)
  (let* ((id (aio-await (1password--search-id)))
         (vault (1password--find-vault id))
         (result (aio-await (1password--read id
                                             "password"
                                             vault))))
    (if result
        (progn
          (kill-new result)
          (message "1Password secret copied to clipboard"))
      (message "1Password entry not found or does not contain a password"))))

;;;###autoload (autoload '1password-delete "1password" nil t)
(aio-defun 1password-delete ()
  "Deletes the selected 1password entry."
  (interactive)
  (let* ((id (aio-await (1password--search-id)))
         (message "%s" id)
         (vault (1password--find-vault id)))
    (aio-await (1password--delete id vault))
    (message "1Password entry deleted")))

;; (aio-defun 1password-generate-password ()
;;   "Generates a random password using 1Password"
;;   (interactive)
;;   (let* ((response (1password--item-create "thing" nil 't)))
;;     (password (gethash "password" response)))
;;   (kill-new password)
;;   (message "1Password generated password copied to clipboard"))

;; TODO: Add support for custom categories
;;;###autoload (autoload '1password-create "1password" nil t)
(aio-defun 1password-create ()
  "Create a new 1Password entry for the Login category.

This method generates defers to 1Password to generate a password using the options '20,letters,digits'"
  (interactive)
  (let* ((template-file (make-temp-file "1password-create.json"))
         (template-buffer (aio-await (1password--fetch-template "Login" template-file))))
    (1password--update-template template-buffer)
    (if (aio-await (1password--create template-bugger template-file))
        (message "1Password entry created")
      (message "Unable to create 1Password entry"))))


;; Local Variables:
;; read-symbol-shorthands: (("op-" . "1password-"))
;; End:
(provide '1password)
;;; 1password.el ends here
