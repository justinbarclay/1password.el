;;; 1password-widget-builder.el --- 1password widget builder for editable forms -*- lexical-binding: t; -*-
(require 'widget)
(require 'json)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: Rework
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define a custom face for section headers
;; inherit fontlock-symbol
(defface 1password-section-header-face
  '((t :weight bold :height 1.2 :inherit magit-section-heading))
  "Face for section headers in the dynamic form.")

(defun 1password--fields-by-section (field-list)
  "Group a flat list of FIELD-LIST into an alist by section.
The returned alist has the format `\'((section-id . (field1 field2...))...)'.
Fields without a section are grouped under the key ':none'."
  (let ((grouped-fields '()))
    (dolist (field field-list)
      (let* ((section-info (plist-get field :section))
             (section-id (if section-info
                             (plist-get section-info :id)
                           :none))
             (section-label (if section-info
                                (plist-get section-info :label)
                              "General")))
        (let ((entry (assoc section-id grouped-fields)))
          (if entry
              (setcdr entry (cons field (cdr entry)))
            (push (list section-id section-label field) grouped-fields)))))
    ;; The list is built backwards, so reverse each sub-list of fields and the main list.
    (mapcar (lambda (group)
              (cons (car group)
                    (cons (cadr group) (nreverse (cddr group)))))
            (nreverse grouped-fields))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Components
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun 1password--section-header (label)
  "Insert a formatted section header into the buffer."
  (unless (string= label "General") ; Don't print a header for the default group
    (widget-insert "\n")
    (widget-insert (propertize (format "%s" label)
                               'face '1password-section-header-face))
    (widget-insert "\n")))

(defun 1password--create-concealed-widget (value on-change)
  "Create a concealed input widget for passwords or PINs."
  (widget-create 'editable-field
                 :size 10
                 :secret t
                 :notify on-change
                 value))

(defun 1password--create-string-widget (value on-change &optional multiline)
  "Create a string widget. If MULTILINE is non-nil, create a notes field."
  (let* ((args (if multiline
                   (list 'editable-field
                         :notify on-change
                         value)

                 (list 'editable-field
                       :size 40
                       :notify on-change
                       value))))
    (apply #'widget-create args)))

(defun 1password--save-button (on-save)
  "Add a Save button to the form."
  (widget-insert "\n")
  (widget-create 'push-button
                 :notify on-save
                 "Save and Close"))

(defun 1password--notify-changes (widget &rest _)
  (let ((new-value (widget-value widget)))
    (setq-local 1password--form-changes
                (plist-put 1password--form-changes id new-value))))

(defun 1password--widget-dispatcher (field)
  "Dispatch to the correct widget creation function based on FIELD-DEF."
  (let* ((id  (plist-get field :id))
         (type  (plist-get field :type))
         (label  (plist-get field :label))
         (value  (plist-get field :value)))
    (when label
      (widget-insert (format "%-25s " label)))
    (pcase type
      ("STRING"
       (1password--create-string-widget value #'1password--notify-changes))
      ("CONCEALED"
       (1password--create-concealed-widget value #'1password--notify-changes))
      ("URL"
       (1password--create-string-widget value #'1password--notify-changes))
      ("ADDRESS"
       (1password--create-string-widget value #'1password--notify-changes 't))
      ("PHONE"
       (1password--create-string-widget value #'1password--notify-changes))
      ;; TODO
      ;; Handle Date
      ;;... other type clauses...
      (_ (message "Warning: Unknown widget type '%s' for id '%s'" type id)))
    (widget-insert "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Form
;;;;;;;;;;;;;;;;;;;;;;;;;
(defun 1password--update-fields (record changes)
  "Update the fields in RECORD with CHANGES."
  (let ((fields (plist-get record :fields)))
    (mapc
     (lambda (field)
       (let ((field-id (plist-get field :id)))
         (if (plist-member changes field-id)
             (plist-put field :value (plist-get changes field-id))
           field)))
     fields))
  record)

(defun 1password--save-item (record changes)
  "Save the current form data to 1Password."
  (let ((temp-file (make-temp-file (format "1password-%s-" (plist-get record :id)))))
    (with-temp-file temp-file
      (let* ((copied-record (copy-sequence record))
             (updated-record (1password--update-fields copied-record changes)))
        (insert (json-encode updated-record))
        (setq-local 1password--form-changes '())
        (kill-current-buffer)))
    temp-file))

(defun 1password--create-form (json-string)
  "Create and display a dynamic form based on JSON-STRING."
  (let* ((record (parse-json-to-plist json-string))
         (form-buffer (format "*1password: %s*" (plist-get record :title))))
    ;; Buffer setup
    (when (get-buffer form-buffer)
      ;; Remove buffer if it previously existed, clearing out the buffer does not work - I get weird
      ;; field exists errors
      (kill-buffer form-buffer))
    (get-buffer-create form-buffer)
    (with-current-buffer form-buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (widget-minor-mode 1)
      (setq-local 1password--form-changes '())
      ;; Form creation
      (let* ((parsed-fields (plist-get record :fields))
             (grouped-fields (1password--fields-by-section parsed-fields))
             (sections (plist-get record :sections)))
        (dolist (section sections)
          (let* ((section-id (plist-get section :id))
                 (section-label (plist-get section :label))
                 (fields (assoc section-id grouped-fields)))

            (1password--section-header section-label)
            (dolist (field fields)
              (1password--widget-dispatcher field))))

        (1password--save-button
         (lambda (&rest _)
           (1password--save-item record 1password--form-changes)))
        (widget-setup)
        (goto-char (point-min)))
      (1password-edit-item-mode)
      (switch-to-buffer form-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major Mode
;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar-local 1password--form-changes '()
  "Local variable to track changes in the form.")

(defvar 1password-edit-item-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") 'kill-current-buffer)
    (define-key map (kbd "C-c C-s") '1password--save-button)
    (define-key map (kbd "q") 'kill-current-buffer)
    map)
  "Keymap for `1password-edit-item-mode'.")

(define-derived-mode 1password-edit-item-mode widget-minor-mode "1Password Edit Item"
  "Major mode for editing 1Password items in a dynamic form."
  :syntax-table nil
  (use-local-map 1password-edit-item-mode-keymap)
  (setq truncate-lines t))

;; (1password--create-form sample-fields)

(provide '1password-widget-builder)
