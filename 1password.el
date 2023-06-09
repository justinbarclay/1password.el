;;; 1password.el --- Emacs ❤️ 1Password -*- lexical-binding: t; -*q-

;; Copyright (C) 2023  Justin Barclay

;; Author: Justin Barclay <emacs@justinbarclay.ca>
;; URL: https://github.com/justinbarclay/1password.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.2"))
;; Keywords: processes

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

(require 'auth-source)
(require 'subr-x)
(require 'json)
(require 'cl-lib)

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

(defcustom 1password--results-formatter '1password--default-formatter
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

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper methods
;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defun 1password--execute-in-buffer (args &optional
                                             (buffer-reader-fn #'json-parse-buffer)
                                             (buffer-name "*1password*"))
  "Run the 1password executable with `ARGS' and processes the JSON response.

`BUFFER-READER-FN' is a function that will be used to process the
output of the call to the 1Password CLI.  By default, this is
`json-parse-buffer'.

`BUFFER-NAME' is the name of the buffer that will house the
1Password process and that 1Password will dump its output too"
  (with-current-buffer (get-buffer-create buffer-name)
    (let ((qualifed-executable (executable-find 1password-executable)))
      (unless qualifed-executable
        (error (format "Unable to find 1Password CLI '%s'" 1password-executable)))
      (read-only-mode -1)
      (erase-buffer)
      (apply #'call-process qualifed-executable nil 't  nil (split-string args " "))
      (special-mode)
      (goto-char (point-min))
      (eval (list buffer-reader-fn)))))

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
               (cl-substitute :login :username)
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
;; 1Password Create
;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defun 1password--create-in-buffer (args &optional
                                             (buffer-reader-fn #'json-parse-buffer)
                                             template-buffer)
  "Sends the contents of the buffer to 1password to create an account.

`ARGS' are the arguments to pass to `op create item'.

`BUFFER-READER-FN' is the function to used to extract data from the buffer.

`TEMPLATE-BUFFER' is that contains the JSON template to send to 1password."
  (let ((qualifed-executable (executable-find 1password-executable))
        (output-buffer (get-buffer-create "*1password*")))
    (unless qualifed-executable
      (error (format "Unable to find 1Password CLI '%s'" 1password-executable)))
    (with-current-buffer output-buffer
      (read-only-mode -1)
      (erase-buffer))
    (with-current-buffer template-buffer
      (read-only-mode -1)
      (apply #'call-process-region
             (point-min)
             (point-max)
             qualifed-executable
             't
             output-buffer
             nil
             (split-string args " ")))
    (with-current-buffer output-buffer
      (special-mode)
      (eval (list buffer-reader-fn)))))

(cl-defun 1password--fetch-template (&optional (category "Login"))
  "Fetches the `op' template for the chosen `CATEGORY'."
  (let ((template-buffer (get-buffer-create "*1password-create*")))
    (1password--execute-in-buffer (string-join (list
                                                "item"
                                                "template"
                                                "get"
                                                category
                                                "--format"
                                                "json")
                                               " ")
                                  'buffer-string
                                  template-buffer)
    template-buffer))

(defun 1password--update-template-fields (fields)
  "Update the `FIELDS' in the buffer with user supplied values."
  (mapcar (lambda (field)
            (let ((field-label (gethash "label" field)))
              (when (and field-label
                         (not (string= field-label "password")))
                (puthash "value"
                         (read-string (format "%s: " field-label))
                         field))
              field))
          fields))

(defun 1password--create (template-buffer)
  "Create a new 1Password entry using the template stored in `TEMPLATE-BUFFER'."
  (1password--create-in-buffer (string-join (list
                                             "item"
                                             "create"
                                             "--generate-password=20,letters,digits")
                                            " ")
                               'buffer-string
                               template-buffer)
      ;; Clear cache and return result
    (setq 1password--item-cache nil))

;; TODO: Add support for other categories
(defun 1password-create (&optional dryrunp)
  "Create a new 1Password entry for the Login category.

If `DRYRUNP' is non-nil it will not persist the entry in
1Password.  This is primarily to use for testing or to have
1Password create a password.

Note: This only supports auto-generated password with 20
characters of Letters and Digits."
  (interactive)
  (let* ((template-buffer (1password--fetch-template))
         template)
    (with-current-buffer template-buffer
      (setq template (json-parse-buffer))
      (puthash "title" (read-string "Entry name: ") template)
      (puthash "fields"
               (apply 'vector
                      (1password--update-template-fields
                       (gethash "fields" template)))
               template)
      (read-only-mode -1)
      (erase-buffer)
      (goto-char (point-min))
      (json-insert template)
      (read-only-mode 1)
      (1password--create template-buffer))
    template))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1Password Share
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Usage:  op item share { <itemName> | <itemID> } [flags]
;; Flags:
;;       --emails strings    Email addresses to share with.
;;       --expiry duration   Link expiring after the specified duration in (s)econds, (m)inutes, or (h)ours (default 7h).
;;   -h, --help              Get help with item share.
;;       --vault string      Look for the item in this vault.
;;       --view-once         Expire link after a single view.
(defun 1password--share (item-id email)
  "Generate a link for `ITEM-ID' that is valid for the given `EMAIL'.

This link will be valid for 7Hours."
       (let ((args (string-join
                    (list "item"
                          "share"
                          item-id
                          "--emails" email)
                    " ")))
         (1password--execute-in-buffer args)))

;; TODO: Add support for custom categories
;; TODO: Add support for more than 1 emails
(defun 1password-share ()
  "Shares the selected 1Password entry to the specified entry."
  (interactive)
  (1password--share (1password--search-id)
                    (read-string "Email: ")))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1Password Get
;;;;;;;;;;;;;;;;;;;;;;;;;
(defun 1password--extract-data (json)
  "Extract the label and value from each entry in `JSON'.

  1Password returns an array of json objects that contain alot of
  meta information about the results.  As a plist this could look
  like:

  ((:id \"username\"
    :type \"STRING\"
    :purpose \"USERNAME\"
    :label \"username\"
    :value \"githubapi@justinbarclay.ca\"
    :reference \"op://Private/api.github.com/username\")
   (:id \"password\"
    :type \"CONCEALED\"
    :purpose \"PASSWORD\"
    :label \"password\"
    :value \"JMH73PktuQK4eCPAvPvc\"
    :entropy 115.5291519165039
    :reference \"op://Private/api.github.com/password\"
    :password_details (:entropy 115
                       :generated t
                       :strength \"FANTASTIC\")))

  When what really matters for auth integration is the `label'
  and `value'

  (list :username \"githubapi@justinbarclay.ca\"
        :password \"JMH73PKTUQK4ECPAVPVC\")

  In reality, we use a hash table to store the data, but it's
  easier to visualize a plist."
  (mapcan
   (lambda (response)
     (list (intern (concat ":" (gethash "label" response)))
           (gethash "value" response)))
   json))

;; Get a CSV list of the username, and password for all logins in a vault:
;;
;;   op item list --categories Login --vault Staging --format=json
;;
;; Selecting a tag '<tag>' will also return items with tags sub-nested to '<tag>'. For example: '<tag/subtag>'.
;;
;; Flags:
;;       --categories categories   Only list items in these categories (comma-separated).
;;       --favorite                Only list favorite items
;;   -h, --help                    Get help with item list.
;;       --include-archive         Include items in the Archive. Can also be set using OP_INCLUDE_ARCHIVE environment variable.
;;       --long                    Output a more detailed item list.
;;       --tags tags               Only list items with these tags (comma-separated).
;;       --vault vault             Only list items in this vault.
(defun 1password--item-list ()
  "Return a list of all items in 1Password."
  (thread-first (string-join
                 '("item"
                   "list"
                   "--format"
                   "json")
                 " ")
                1password--execute-in-buffer))

(defun 1password--cached-item-list ()
  "Return the cached list of 1Password entries.

If no cached entries are found, it retrieves the current list
from the 1Password CLI."
  (if (bound-and-true-p 1password--item-cache)
      1password--item-cache
    (setq 1password--item-cache (1password--item-list))))

(defun 1password--search-id (&optional ids)
  "Search a cached list of 1Password entries for entries with `IDS'."
  (let* ((candidates (apply 1password--results-formatter
                            (list (1password--cached-item-list))))
         (response (completing-read "1Password title: "
                                    candidates
                                    (when ids
                                      (lambda (entry)
                                        (member (cadr entry) ids)))
                                    't)))
    (cadr (assoc response candidates))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Candidate Formatting
;;;;;;;;;;;;;;;;;;;;;;;;;
(defun 1password--nested-get (fields candidate &optional dflt)
  (if (stringp fields)
      (gethash fields candidate)
    (or (cl-reduce (lambda (field field-name)
                  (and
                   field
                   (gethash field-name field nil)))
                fields
                :initial-value candidate)
        dflt)))

(defun 1password--nested-put-helper (fields value candidate)
  (if (= (length fields) 1)
      (progn
        (puthash (car fields) value candidate)
        candidate)
    (let ((field (or (gethash (car fields) candidate)
                     (make-hash-table :test 'equal))))
      (puthash (car fields)
               (1password--nested-put-helper (cdr fields)
                                             value
                                             field)
               candidate)
      candidate)))

(defun 1password--nested-put (fields value candidate)
  (if (stringp fields)
      (progn
        (puthash fields value candidate)
        candidate)
    (1password--nested-put-helper fields
                                  value
                                  candidate)))

(defun 1password--max-candidate-lengths (candidates fields)
  "Return a hash-table of the max length of each `FIELDS' in `CANDIDATES'.
  - `CANDIDATES' is a hash
  - `FIELDS' is a list of fields to check for the max length"
  (cl-reduce (lambda (acc candidate)
               (dolist (field fields)
                 (if (listp field)
                     (1password--nested-put field
                                            (max
                                             (1password--nested-get field acc 0)
                                             (length (1password--nested-get field candidate "")))
                                            acc)
                   (puthash field
                            (max
                             (gethash field acc 0)
                             (length (gethash field candidate "")))
                            acc)))
               acc)
             candidates
             :initial-value (make-hash-table :test 'equal :size (length fields))))

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

(setq 1password--results-formatter '1password-colour-formatter)

(defun 1password-default-formatter (results)
  "Format the cached `RESULTS' from 1Password for Minibuffer Completion."
  (mapcar
   (lambda (response)
     (list
      (gethash "title" response)
      (gethash "id" response)))
   results))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1Password Read
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Get a database password, stored in vault 'app-prod' in item 'db' at field 'password':
;;
;;         op read op://app-prod/db/password
;; Flags:
;;       --file-mode filemode   Set filemode for the output file. It is ignored without the --out-file flag. (default 0600)
;;   -f, --force                Do not prompt for confirmation.
;;   -h, --help                 Get help with read.
;;   -n, --no-newline           Do not print a new line after the secret.
;;   -o, --out-file string      Write the secret to a file instead of stdout.
(cl-defun 1password--read (&rest spec &key vault entry-id field)
  "Read a `FIELD' from 1Password using properties found in `SPEC'.

- `VAULT' is the vault where the entry is stored
- `ENTRY-ID' is the id of the entry"
  (let ((args (string-join (list
                            "read op:/"
                            vault
                            entry-id
                            field)
                           "/")))
    (1password--execute-in-buffer args 'buffer-string)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delete Commands
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Move an item to the Archive:
;;
;;     op item delete "Defunct Login" --archive
;;
;; Flags:
;;       --archive        Move the item to the Archive.
;;   -h, --help           Get help with item delete.
;;       --vault string   Look for the item in this vault.
(cl-defun 1password--delete (&rest spec &key entry-id vault)
  "Delete a 1Password entry that matches `SPEC'.

  - `VAULT' is the vault where the entry is stored
  - `ENTRY-ID' is the id of the entry"
  (let* ((args (string-join (list
                            "item"
                            "delete"
                            entry-id
                            "--archive"
                            (when vault (format "--vault %s" vault)))
                           " "))
         (result (1password--execute-in-buffer args 'buffer-string)))
    ;; Clear cache and return result
    (setq 1password--item-cache nil)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Commands
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun 1password-enable-auth-source ()
  "Enable 1Password integration with auth-source."
  (interactive)
  (add-hook 'auth-source-backend-parser-functions #'1password-auth-source-backend-parse)
  (add-to-list 'auth-sources '1password))

;; (defun 1password-disable-auth-source ()
;;   "Remove 1Password from auth-source integration")

;;;###autoload
(defun 1password-search-id ()
  "Search for 1Password id by entry name by title."
  (interactive)
  (kill-new (1password--search-id))
  (message "1Password ID copied to clipboard"))

;;;###autoload
(defun 1password-search-password ()
  "Search for password by entry name."
  (interactive)
  (let* ((id (1password--search-id))
         (vault (thread-last (cl-find id 1password--item-cache :test
                                      (lambda (target item)
                                        (eq (gethash "id" item) target)))
                             (gethash "vault")
                             (gethash "name"))))
    (kill-new (1password--read :vault vault :entry-id id :field "password"))
    (message "1Password secret copied to clipboard")))

;;;###autoload
(defun 1password-delete ()
  "Deletes the selected 1password entry."
  (interactive)
  (let* ((id (1password--search-id))
        (vault (thread-last (cl-find id 1password--item-cache :test
                                     (lambda (target item)
                                       (eq (gethash "id" item) target)))
                            (gethash "vault")
                            (gethash "name"))))
    (1password--delete :entry-id id :vault vault)
    (message "1Password entry deleted")))

;; (defun 1password-generate-password ()
;;   "Generates a random password using 1Password"
;;   (interactive)
;;   (let* ((response (1password--item-create "thing" nil 't)))
;;     (password (gethash "password" response)))
;;   (kill-new password)
;;   (message "1Password generated password copied to clipboard"))

;; Local Variables:
;; read-symbol-shorthands: (("op-" . "1password-"))
;; End:
(provide '1password)
;;; 1password.el ends here
