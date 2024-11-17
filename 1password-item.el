;;; 1password-item.el --- 1password functions for interacting with the item subcommand -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '1password-lib)
(require 'aio)
(require 'cl-lib)
(require 'json)
(require 'subr-x)
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

(aio-defun 1password--fetch-template (category &optional buffer-name)
  "Fetches the `op' template for the chosen `CATEGORY'."
  (let ((template-buffer (get-buffer-create (or buffer-name
                                                "*1password-template*"))))
    (aio-await (1password--execute-in-buffer-async
                (string-join (list
                              "item"
                              "template"
                              "get"
                              category
                              "--format"
                              "json")
                             " ")
                'identity
                template-buffer))
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

(aio-defun 1password--create (template-file &optional dryrunp)
  "Create a new 1Password entry using the template stored in `TEMPLATE-BUFFER'.

When `DRYRUNP' the new entry will not persist the entry in
1Password, instead the password will be returned."
  (with-current-buffer template-buffer
      (set-visited-file-name template-file 'nil)
      (setq buffer-save-without-query 't)
      (write-region
       nil
       nil
       template-file))
  (aio-await
   (1password--execute-in-buffer-async
    (string-join (list
                  "item"
                  "create"
                  "--template"
                  template-file
                  "--generate-password=20,letters,digits")
                 " ")
    'identity))
    ;; Clear cache and return result
  (setq 1password--item-cache nil))

(defun 1password--update-template (template-buffer)
  "Update the `TEMPLATE-BUFFER' with user supplied values."
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
      (read-only-mode 1)))

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
(aio-defun 1password--share (item-id email)
  "Generate a link for `ITEM-ID' that is valid for the given `EMAIL'.

This link will be valid for 7Hours."
       (let ((args (string-join
                    (list "item"
                          "share"
                          item-id
                          "--emails" email)
                    " ")))
         (aio-await (1password--execute-in-buffer-async args 'identity))))

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
(aio-defun 1password--item-list ()
  "Return a list of all items in 1Password."
  (thread-first (string-join
                 '("item"
                   "list"
                   "--format"
                   "json")
                 " ")
                1password--execute-in-buffer-async
                aio-await))

(aio-defun 1password--cached-item-list ()
  "Return the cached list of 1Password entries.

If no cached entries are found, it retrieves the current list
from the 1Password CLI."
  (if (bound-and-true-p 1password--item-cache)
      1password--item-cache
    (setq 1password--item-cache (aio-await (1password--item-list)))))

(aio-defun 1password--search-id (&optional ids)
  "Search a cached list of 1Password entries for entries with `IDS'."
  (let* ((candidates (aio-await (1password--cached-item-list)))
         (formatted-candidates (funcall 1password-results-formatter
                                        candidates))
         (response (completing-read "1Password title: "
                                    formatted-candidates
                                    (when ids
                                      (lambda (entry)
                                        (member (cadr entry) ids)))
                                    't)))
    (cadr (assoc response formatted-candidates))))

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
(aio-defun 1password--read (entry-id field vault)
  "Read a `FIELD' from 1Password using properties found in `SPEC'.

- `VAULT' is the vault where the entry is stored
- `ENTRY-ID' is the id of the entry"
  (let ((args (string-join (list
                            "read op:/"
                            vault
                            entry-id
                            field)
                           "/")))
    (aio-await (1password--execute-in-buffer-async args 'identity))))

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

(aio-defun 1password--delete (entry-id vault)
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
         (message "delete")
         (result (aio-await (1password--execute-in-buffer-async args 'identity))))
    ;; Clear cache and return result
    (setq 1password--item-cache nil)
    result))

;; Local Variables:
;; read-symbol-shorthands: (("op--item" . "1password--item"))
;; End:
(provide '1password-item)
;;; 1password-item.el ends here
