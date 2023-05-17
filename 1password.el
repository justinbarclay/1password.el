;;; 1password.el --- Emacs ❤️ 1Password -*- lexical-binding: t; -*-

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

;;; Code :
(require 'auth-source)
(require 'subr-x)
(require 'json)
(require 'cl-lib)

(defgroup 1password nil
  "1Password integration for Emacs"
  :group 'applications)

(defcustom 1password-executable (if (eq system-type 'windows-nt)
                                    "op.exe"
                                  "op")
  "The fully qualified path to the 1Password executable."
  :type '(string)
  :options '("op" "op.exe")
  :group '1password)

(defvar 1password--item-cache nil "A cache for 1Password's `item list' command")

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper methods
;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defun 1password--execute-in-buffer (args &optional (buffer-reader-fn #'json-parse-buffer))
  "Runs 1password executable with `args' and processes the JSON response"
  (with-current-buffer (get-buffer-create "*1password*")
    (let ((qualifed-executable (executable-find 1password-executable)))
      (unless qualifed-executable
        (error (format "Unable to find 1Password CLI '%s'" 1password-executable)))
      (read-only-mode -1)
      (erase-buffer)
      (apply #'call-process qualifed-executable nil 't  nil (split-string args " "))
      (special-mode)
      (goto-char (point-min))
      (eval (list buffer-reader-fn)))))

(cl-defun 1password--query-builder (&key id-or-name
                                         (field-keys '(username password))
                                         vault
                                         &allow-other-keys)
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
  "Search function for use with 1Password"
  (condition-case json-error
      (thread-first (apply #'1password--query-builder args)
                    1password--execute-in-buffer
                    1password--get-to-plist)
    (error (if-let* ((ids (1password--parse-buffer-for-ids))
                     (id (1password--search-id ids)))
               (thread-first (apply #'1password--query-builder :id-or-name id :field-keys field-keys :vault vault)
                             1password--execute-in-buffer
                             1password--get-to-plist)
             json-error))))

(defun 1password--parse-buffer-for-ids ()
  "Returns a list of all ids mentioned in the error response from
the 1Password CLI

When searching for entries in 1Password, if the query is not
specific enough it is possible for 1Password to find multiple
entries. We can parse out these entries and try to give the user
a second chance.

The buffer that we are looking for will look something like this:
[ERROR] 2023/05/20 00:20:53 More than one item matches \"login\".
Try again and specify the item by its ID:
	* for the item \"Login\" in vault Private: ai46l2ccrszcvygm2v6lltgchq
	* for the item \"Login\" in vault Private: gijwlc4a2iipno6rm47sx6bxni
	* for the item \"Login\" in vault Private: j6f64bfgbg3wssv6lklmsc6ayu
	* for the item \"Login\" in vault Private: hpn47a3vncdtz77wg7eg6ix6ea

(1password--parse-buffer-for-ids)

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
  "Runs 1Passwords `get item' command on the `host' or `id' key

If both `id' and `host' are passed in, it will preferentialy
search by id.

If the host you are searching for has more than 1 entry within
1Password, `1password--auth-source-search' will throw a parsing
error. If this is the case it is recommended that you set the
host to be a unique name or use the 1Password ID for the item.

You can use `1password-search-id' to find the id for of an entry."
  (thread-last (1password--search :id-or-name (or host id))
               (cl-substitute :secret :password)
               (append (list :backend '1password :host host))))

(defvar 1password-auth-source-backend
  (auth-source-backend
   :source "."
   :type '1password
   :search-function #'1password--auth-source-search))

(defun 1password-auth-source-backend-parse (entry)
  (when (eq entry '1password)
    (auth-source-backend-parse-parameters entry 1password-auth-source-backend)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1Password Item
;;;;;;;;;;;;;;;;;;;;;;;;;
(defun 1password--get-to-plist (json)
  "Extracts the label and value from each object and returns a
  simple plist

  1Password returns an array of json objects that contain alot of
  meta information about the results. As a plist this could look
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

  "
  (mapcan
   (lambda (response)
     (list (intern (concat ":" (gethash "label" response)))
           (gethash "value" response)))
   json))

(defun 1password--item-list ()
  (thread-first (string-join
                 '("item"
                   "list"
                   "--format"  "json")
                 " ")
                1password--execute-in-buffer))

(defun 1password--cached-item-list ()
  "Returns the cached list of 1Password entries

If no cached entries are found, it retrieves the current list
from the 1Password CLI."
  (if (bound-and-true-p 1password--item-cache)
      1password--item-cache
    (setq 1password--item-cache (1password--item-list))))

(defun 1password--search-id (&optional ids)
  (let* ((candidates (1password--format-list
                      (1password--cached-item-list)))
         (response (completing-read "1Password title: "
                                    candidates
                                    (when ids
                                      (lambda (entry)
                                        (member (cadr entry) ids)))
                                    't)))
    (cadr (assoc response candidates))))

(defun 1password--format-list (results)
  "Formats the cached results from 1Password CLI for the Minibuffer Completion"
  (mapcar
   (lambda (response)
     (list
      (format "%s\t(%s)" (gethash "title" response) (gethash "additional_information" response))
      (gethash "id" response)))
   results))
;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1Password Read
;;;;;;;;;;;;;;;;;;;;;;;;;
(defun 1password--read-to-string ()
  "")

(cl-defun 1password--read (&rest spec &key vault entry-id field)
  (let ((args (string-join (list
                            "read op:/"
                            vault
                            entry-id
                            field)
                           "/")))
    (1password--execute-in-buffer args 'buffer-string)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Commands
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun 1password-enable-auth-source ()
  "Enable 1Password integration with auth-source"
  (interactive)
  (add-hook 'auth-source-backend-parser-functions #'1password-auth-source-backend-parse)
  (add-to-list 'auth-sources '1password))

;; (defun 1password-disable-auth-source ()
;;   "Remove 1Password from auth-source integration")

;;;###autoload
(defun 1password-search-id ()
  "Search for 1Password id by entry name by title"
  (interactive)
  (kill-new (1password--search-id))
  (message "1Password ID copied to clipboard"))

;;;###autoload
(defun 1password-search-password ()
  "Search for password by entry name"
  (interactive)
  (let* ((id (1password--search-id))
         (vault (thread-last (cl-find id 1password--item-cache :test
                                      (lambda (target item)
                                        (eq (gethash "id" item) target)))
                             (gethash "vault")
                             (gethash "name"))))
    (kill-new (1password--read :vault vault :entry-id id :field "password"))
    (message "1Password secret copied to clipboard")))

(provide '1password)
;;; 1password.el ends here
