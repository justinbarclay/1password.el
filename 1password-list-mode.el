;;; 1password-list-mode.el --- A major mode for listing items in 1password  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Barclay

;; Author: Justin Barclay <github@justincbarclay.ca>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'tabulated-list)
(require '1password)
(require 'aio)

;; The format of the current Tabulated List mode buffer.
;; This should be a vector of elements (NAME WIDTH SORT . PROPS),
;; where:
;;  - NAME is a string describing the column.
;;    This is the label for the column in the header line.
;;    Different columns must have non-`equal' names.
;;  - WIDTH is the width to reserve for the column.
;;    For the final element, its numerical value is ignored.
;;  - SORT specifies how to sort entries by this column.
;;    If nil, this column cannot be used for sorting.
;;    If t, sort by comparing the string value printed in the column.
;;    Otherwise, it should be a predicate function suitable for
;;    `sort', accepting arguments with the same form as the elements
;;    of `tabulated-list-entries'.
;;  - PROPS is a plist of additional column properties.
;;    Currently supported properties are:
;;    - `:right-align': If non-nil, the column should be right-aligned.
;;    - `:pad-right': Number of additional padding spaces to the
;;      right of the column (defaults to 1 if omitted)."
(defvar 1password-list-format [("Title" 30 t)
                               ("Category" 15 t)
                               ("Vault" 10 t)
                               ("Additional Information" 10 t)
                               ("Edited" 10 t)]
  "Format of the 1password list.")

(defun 1password-list--maximize-columns (title category vault additional-information)
  "Maximize the columns `TITLE', `CATEGORY', `VAULT', and `ADDITIONAL-INFORMATION'."
  (setq 1password-list-format (vector (list "Title"
                                            (max (nth 1 (aref 1password-list-format 0))
                                                 (length title))
                                            t)
                                      (list "Category"
                                            (max (nth 1 (aref 1password-list-format 1))
                                                 (length category))
                                            t)
                                      (list "Vault"
                                            (max (nth 1 (aref 1password-list-format 2))
                                                 (length vault))
                                            t)
                                      (list "Additional Information"
                                            (max (nth 1 (aref 1password-list-format 3))
                                                 (length additional-information))
                                            t)
                                      (list "Edited"
                                            10
                                            t))))

(defun 1password-list--to-entries (item)
  "Convert `ITEM' to a tabulated list entry."
  (let ((id (gethash "id" item))
        (title (gethash "title" item))
        (category (gethash "category" item))
        (vault (thread-last item
                            (gethash "vault")
                            (gethash "name")))
        (additional-information (or (gethash "additional_information" item) ""))
        (edited (gethash "updated_at" item)))
    (1password-list--maximize-columns title category vault additional-information)
    (list id (vector title category vault additional-information edited))))

(aio-defun 1password-list--refresh (&optional _arg _noconfirm)
  "Refresh the 1password list."
  (setq mode-line-process " Refreshing...")
  (let ((entries (aio-await (1password--cached-item-list))))
    (setq tabulated-list-entries (mapcar #'1password-list--to-entries entries))
    (tabulated-list-init-header)
    (tabulated-list-print)
    (setq mode-line-process nil)))

(aio-defun list-1password-item ()
  "Display a list of items from 1password."
  (interactive)
  (let ((buf (get-buffer-create "*1Password Items*"))
        (entries (aio-await (1password--cached-item-list))))
    (with-current-buffer buf
      (setq tabulated-list-entries (mapcar #'1password-list--to-entries entries))
      (setq buffer-file-coding-system 'utf-8)
      (1password-list-mode)
      (tabulated-list-init-header)
      (tabulated-list-print 't)
      (switch-to-buffer buf))))

(aio-defun 1password-list-visit-item ()
  "Visit the 1Password entry at point."
  (interactive nil 1password-list-mode)
  (aio-await (1password--view (tabulated-list-get-id))))

(aio-defun 1password-list-share ()
  "Share the 1Password entry at point."
  (interactive nil 1passwosrd-list-mode)
  (aio-await (1password-share (tabulated-list-get-id))))

(defvar-keymap 1password-list-mode-map
  :doc "Local keymap for `1password-list-mode' buffers."
  :parent tabulated-list-mode-map
  "S"   #'1password-list-share
  "RET" #'1password-list--visit-item)

;;   "Generic major mode for browsing a list of items.
;; This mode is usually not used directly; instead, other major
;; modes are derived from it, using `define-derived-mode'.

;; In this major mode, the buffer is divided into multiple columns,
;; which are labeled using the header line.  Each non-empty line
;; belongs to one \"entry\", and the entries can be sorted according
;; to their column values.

;; An inheriting mode should usually do the following in their body:

;;  - Set `tabulated-list-format', specifying the column format.
;;  - Set `tabulated-list-revert-hook', if the buffer contents need
;;    to be specially recomputed prior to `revert-buffer'.
;;  - Maybe set a `tabulated-list-entries' function (see below).
;;  - Maybe set `tabulated-list-printer' (see below).
;;  - Maybe set `tabulated-list-padding'.
;;  - Call `tabulated-list-init-header' to initialize `header-line-format'
;;    according to `tabulated-list-format'.

;; An inheriting mode is usually accompanied by a \"list-FOO\"
;; command (e.g. `list-packages', `list-processes').  This command
;; creates or switches to a buffer and enables the major mode in
;; that buffer.  If `tabulated-list-entries' is not a function, the
;; command should initialize it to a list of entries for displaying.
;; Finally, it should call `tabulated-list-print'.

;; `tabulated-list-print' calls the printer function specified by
;; `tabulated-list-printer', once for each entry.  The default
;; printer is `tabulated-list-print-entry', but a mode that keeps
;; data in an ewoc may instead specify a printer function (e.g., one
;; that calls `ewoc-enter-last'), with `tabulated-list-print-entry'
;; as the ewoc pretty-printer."
(define-derived-mode 1password-list-mode tabulated-list-mode "op items"
  "Major Mode for viewing items from a vault in one password."
  :interactive nil
  :map 1password-list-mode-map
  (setq tabulated-list-format 1password-list-format)
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Title" nil))
  (tabulated-list-init-header)
  (setq revert-buffer-function '1password-list--refresh)
  ;; TODO: Maybe consider imenu support
  ;; (setf imenu-prev-index-position-function
  ;;       #'package--imenu-prev-index-position-function)
  ;; (setf imenu-extract-index-name-function
  ;;       #'package--imenu-extract-index-name-function)
  )

(provide '1password-list-mode)
;;; 1password-list-mode.el ends here
