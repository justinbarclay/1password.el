;;; 1password-age.el --- age.el integration for 1Password -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Justin Barclay

;; Author: Justin Barclay <emacs@justinbarclay.ca>
;; URL: https://github.com/justinbarclay/1password.el
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.2") (age "0.1.4") (aio "1.0"))
;; Keywords: tools, processes

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
;;
;; This package extends age.el so that age identities and recipients
;; are fetched from 1Password instead of living on disk.
;;
;; Enable `1password-age-mode' and age.el's normal workflow — visiting
;; and saving `.age' files, `age-decrypt-file', org-roam integration,
;; and so on — transparently uses keys stored in 1Password:
;;
;; - Decryption: the private key is retrieved from 1Password, written
;;   to a temporary file (0600, in `/dev/shm' when available so it
;;   never touches persistent storage), handed to the age binary, and
;;   deleted as soon as the operation completes.
;;
;; - Encryption: the public key is passed to the age binary as a raw
;;   `-r' string.  It never touches disk at all.
;;
;; Two kinds of 1Password items are supported:
;;
;; - SSH Key items: the native 1Password category.  The `private_key'
;;   and `public_key' fields are used.
;;
;; - Native age keys (`AGE-SECRET-KEY-1...') stored in a Password or
;;   Secure Note item.  The secret key is read from the field named by
;;   `1password-age-key-field' and the public key (`age1...') from the
;;   field named by `1password-age-recipient-field'.
;;
;; Setup:
;;
;;   (use-package 1password-age
;;     :after age
;;     :custom
;;     (1password-age-identity-item  "your-item-title-or-id")
;;     (1password-age-recipient-item "your-item-title-or-id")
;;     :config
;;     (1password-age-mode 1))
;;
;; When the item options are nil you are prompted to pick an item the
;; first time a key is needed.  Both options are safe as file-local or
;; dir-local variables, so different directories can use different
;; keys via `.dir-locals.el'.
;;
;; Known limitation: on Windows there is no `/dev/shm' and
;; `set-file-modes' cannot restrict permissions, so the temporary
;; identity file is only protected by its short lifetime.

;;; Code:

(require '1password)
(require 'age)
(require 'aio)
(require 'cl-lib)

(defgroup 1password-age nil
  "Age encryption integration for 1Password."
  :group '1password
  :group 'age)

(defcustom 1password-age-identity-item nil
  "1Password item holding the private key used for decryption.
The item's title or ID.  When nil, prompt for an item the first
time a decryption needs it."
  :type '(choice (const :tag "Prompt when needed" nil)
                 (string :tag "1Password item title or ID"))
  :safe #'stringp
  :group '1password-age)

(defcustom 1password-age-recipient-item nil
  "1Password item holding the public key used for encryption.
The item's title or ID.  When nil, prompt for an item the first
time an encryption needs it."
  :type '(choice (const :tag "Prompt when needed" nil)
                 (string :tag "1Password item title or ID"))
  :safe #'stringp
  :group '1password-age)

(defcustom 1password-age-key-field "password"
  "Field label holding an age secret key in non-SSH 1Password items.
Only consulted for items that are not in the SSH Key category."
  :type 'string
  :group '1password-age)

(defcustom 1password-age-recipient-field "public_key"
  "Field label holding the public key in non-SSH 1Password items.
Only consulted for items that are not in the SSH Key category."
  :type 'string
  :group '1password-age)

(defcustom 1password-age-temp-directory
  (if (file-directory-p "/dev/shm/")
      "/dev/shm/"
    temporary-file-directory)
  "Directory for temporary identity files.
Defaults to /dev/shm (memory-backed) when available so private
keys never touch persistent storage."
  :type 'directory
  :group '1password-age)

(defcustom 1password-age-timeout 60
  "Seconds to wait for the 1Password CLI before giving up.
Retrieving a key can require unlocking 1Password (for example via
a biometric prompt), so this should be generous."
  :type 'natnum
  :group '1password-age)

(defvar 1password-age--temp-files nil
  "Temporary identity files not yet cleaned up.
Normally deleted by the `unwind-protect' that created them; this
list is a backstop for `kill-emacs-hook'.")

(defvar 1password-age--bypass nil
  "When non-nil, `1password-age-mode' advice passes through untouched.
Bound by commands that manage their own key material.")

;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1Password access
;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar 1password-age--key-categories '("SSH_KEY" "PASSWORD" "SECURE_NOTE")
  "1Password item categories that may hold age-usable keys.")

(defun 1password-age--wait (promise)
  "Synchronously wait for PROMISE, honouring `1password-age-timeout'.
CLI failures may surface as timeouts rather than immediate
errors: `1password--execute-async' signals inside its process
sentinel on failure, which never rejects the promise."
  (with-timeout (1password-age-timeout
                 (error "1Password CLI timed out after %ss" 1password-age-timeout))
    (aio-wait-for promise)))

(aio-defun 1password-age--key-items ()
  "Return cached 1Password items that may hold age-usable keys."
  (let ((items (aio-await (1password--cached-item-list))))
    (cl-remove-if-not
     (lambda (item)
       (member (gethash "category" item "") 1password-age--key-categories))
     items)))

(defun 1password-age--select-key-item (prompt)
  "Prompt with PROMPT for a 1Password key item and return its ID."
  (let* ((items (1password-age--wait (1password-age--key-items)))
         (candidates (funcall 1password-results-formatter items))
         (selected (completing-read prompt candidates nil t)))
    (cadr (assoc selected candidates))))

(defun 1password-age--find-item (id-or-name)
  "Return the cached 1Password item matching ID-OR-NAME.
Matches on item ID or title, populating the item cache when
empty.  Signal an error when nothing matches."
  (let ((items (1password-age--wait (1password--cached-item-list))))
    (or (cl-find id-or-name items
                 :test (lambda (target item)
                         (or (equal (gethash "id" item) target)
                             (equal (gethash "title" item) target))))
        (error "No 1Password item matches `%s'" id-or-name))))

(defun 1password-age--read-field (id-or-name field)
  "Synchronously read FIELD from the 1Password item ID-OR-NAME."
  (let ((item (1password-age--find-item id-or-name)))
    (string-trim
     (1password-age--wait
      (1password--read (gethash "id" item)
                       field
                       (gethash "name" (gethash "vault" item)))))))

(defun 1password-age--item-category (id-or-name)
  "Return the category of the 1Password item matching ID-OR-NAME."
  (gethash "category" (1password-age--find-item id-or-name)))

(defun 1password-age--identity-string (id-or-name)
  "Return the private key stored in the 1Password item ID-OR-NAME."
  (let* ((field (if (equal (1password-age--item-category id-or-name) "SSH_KEY")
                    "private_key"
                  1password-age-key-field))
         (key (1password-age--read-field id-or-name field)))
    (unless (or (string-prefix-p "AGE-SECRET-KEY-1" key)
                (string-match-p "PRIVATE KEY-----" key))
      (error "Field `%s' of item `%s' does not look like a private key"
             field id-or-name))
    key))

(defun 1password-age--recipient-string (id-or-name)
  "Return the public key stored in the 1Password item ID-OR-NAME."
  (let* ((field (if (equal (1password-age--item-category id-or-name) "SSH_KEY")
                    "public_key"
                  1password-age-recipient-field))
         (key (1password-age--read-field id-or-name field)))
    (unless (string-match-p "\\`\\(age1\\|ssh-ed25519 \\|ssh-rsa \\)" key)
      (error "Field `%s' of item `%s' does not look like an age recipient"
             field id-or-name))
    key))

(defun 1password-age--identity-item ()
  "Return the configured identity item, prompting when unset."
  (or 1password-age-identity-item
      (1password-age--select-key-item "Identity (private key) item: ")))

(defun 1password-age--recipient-item ()
  "Return the configured recipient item, prompting when unset."
  (or 1password-age-recipient-item
      (1password-age--select-key-item "Recipient (public key) item: ")))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Temporary identity files
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun 1password-age--write-temp-key (key)
  "Write KEY to a private temporary file and return its path."
  (let* ((temporary-file-directory 1password-age-temp-directory)
         (temp-file (make-temp-file "1password-age-")))
    (set-file-modes temp-file #o600)
    (push temp-file 1password-age--temp-files)
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region (if (string-suffix-p "\n" key) key (concat key "\n"))
                    nil temp-file nil 'silent))
    temp-file))

(defun 1password-age--delete-temp-key (file)
  "Delete the temporary identity FILE."
  (when (and file (file-exists-p file))
    (delete-file file))
  (setq 1password-age--temp-files (delete file 1password-age--temp-files)))

(defun 1password-age--cleanup-temp-files ()
  "Delete any temporary identity files that escaped their unwind."
  (dolist (file 1password-age--temp-files)
    (when (file-exists-p file)
      (delete-file file)))
  (setq 1password-age--temp-files nil))

(add-hook 'kill-emacs-hook #'1password-age--cleanup-temp-files)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; age.el advice
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun 1password-age--around-start-decrypt (orig-fn context cipher)
  "Fetch the identity from 1Password before running ORIG-FN.
CONTEXT and CIPHER are as in `age-start-decrypt'.  The identity is
written to a temporary file, the operation is run to completion,
and the file is deleted — `age-start-decrypt' only starts the age
process, so completion must be awaited here to avoid deleting the
key while the process is still reading it."
  (if (or 1password-age--bypass (age-context-passphrase context))
      (funcall orig-fn context cipher)
    (let* ((key (1password-age--identity-string (1password-age--identity-item)))
           (temp-key (1password-age--write-temp-key key)))
      (unwind-protect
          (let ((age-default-identity temp-key)
                (age-always-use-default-keys t))
            (funcall orig-fn context cipher)
            (age-wait-for-completion context))
        (1password-age--delete-temp-key temp-key)))))

(defun 1password-age--around-start-encrypt (orig-fn context plain recipients)
  "Fetch the recipient from 1Password before running ORIG-FN.
CONTEXT, PLAIN and RECIPIENTS are as in `age-start-encrypt'.  When
RECIPIENTS is already provided by the caller it is respected.  The
public key is passed as a raw string, so no temporary file is
involved."
  (if (or 1password-age--bypass recipients (age-context-passphrase context))
      (funcall orig-fn context plain recipients)
    (let ((age-default-recipient
           (1password-age--recipient-string (1password-age--recipient-item)))
          (age-always-use-default-keys t))
      (funcall orig-fn context plain recipients))))

;;;###autoload
(define-minor-mode 1password-age-mode
  "Fetch age.el keys from 1Password.

When enabled, decryption retrieves the private key from
1Password and hands it to the age binary via a short-lived
temporary file in `1password-age-temp-directory'.  Encryption
retrieves the public key and passes it as a raw string.

Which items keys come from is controlled by
`1password-age-identity-item' and `1password-age-recipient-item';
when unset you are prompted the first time a key is needed."
  :global t
  :lighter " 1P-age"
  :group '1password-age
  (if 1password-age-mode
      (progn
        (advice-add 'age-start-decrypt :around #'1password-age--around-start-decrypt)
        (advice-add 'age-start-encrypt :around #'1password-age--around-start-encrypt))
    (advice-remove 'age-start-decrypt #'1password-age--around-start-decrypt)
    (advice-remove 'age-start-encrypt #'1password-age--around-start-encrypt)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive commands
;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun 1password-age-encrypt-file (file &optional output)
  "Encrypt FILE to OUTPUT using a public key from 1Password.
Interactively, prompt for FILE and OUTPUT, defaulting OUTPUT to
FILE with an `.age' extension.  Works whether or not
`1password-age-mode' is enabled."
  (interactive
   (let ((file (read-file-name "File to encrypt: " nil nil t)))
     (list file (read-file-name "Save encrypted file as: "
                                nil nil nil
                                (concat (file-name-nondirectory file) ".age")))))
  (let ((output (or output (concat file ".age")))
        (recipient (1password-age--recipient-string (1password-age--recipient-item)))
        (1password-age--bypass t))
    (age-encrypt-file (age-make-context 'Age) file (list recipient) output)
    (message "Encrypted %s to %s" file output)))

;;;###autoload
(defun 1password-age-decrypt-file (file &optional output)
  "Decrypt FILE to OUTPUT using a private key from 1Password.
Interactively, prompt for FILE and OUTPUT, defaulting OUTPUT to
FILE without its `.age' extension.  Works whether or not
`1password-age-mode' is enabled."
  (interactive
   (let ((file (read-file-name "File to decrypt: " nil nil t)))
     (list file (read-file-name "Save decrypted file as: "
                                nil nil nil
                                (file-name-nondirectory
                                 (if (string-suffix-p ".age" file)
                                     (string-remove-suffix ".age" file)
                                   (concat file ".decrypted")))))))
  (let* ((output (or output (string-remove-suffix ".age" file)))
         (key (1password-age--identity-string (1password-age--identity-item)))
         (temp-key (1password-age--write-temp-key key))
         (1password-age--bypass t))
    (unwind-protect
        (let ((age-default-identity temp-key)
              (age-always-use-default-keys t))
          (age-decrypt-file (age-make-context 'Age) file output)
          (message "Decrypted %s to %s" file output))
      (1password-age--delete-temp-key temp-key))))

;;;###autoload
(defun 1password-age-select-identity ()
  "Select and save the default 1Password item for age identities."
  (interactive)
  (let ((item-id (1password-age--select-key-item "Default identity item: ")))
    (customize-save-variable '1password-age-identity-item item-id)
    (message "1Password age identity set to %s" item-id)))

;;;###autoload
(defun 1password-age-select-recipient ()
  "Select and save the default 1Password item for age recipients."
  (interactive)
  (let ((item-id (1password-age--select-key-item "Default recipient item: ")))
    (customize-save-variable '1password-age-recipient-item item-id)
    (message "1Password age recipient set to %s" item-id)))

;; Local Variables:
;; read-symbol-shorthands: (("op-" . "1password-"))
;; End:

(provide '1password-age)
;;; 1password-age.el ends here
