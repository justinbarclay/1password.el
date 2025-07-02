;;; 1password-widget-builder.el --- 1password widget builder for editable forms -*- lexical-binding: t; -*-

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
;; The rest
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parse this json to plist
(defun parse-json-to-plist (json-string)
  "Parse JSON STRING and return a plist."
  (let ((json-object-type 'plist))
    (json-parse-string json-string
                       :object-type 'plist
                       :array-type 'list)))

(defun 1password--extract-values (widget-map)
  "Extract all values from the form widgets into an alist.
The alist maps field IDs (as strings) to their values."
  (let ((results '()))
    (maphash (lambda (id widget)
               (plist-put results id (widget-value widget)))
             widget-map)
    results))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Components
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun 1password--create-concealed-widget (id label value widget-map)
  "Create a concealed input widget for passwords or PINs."
  (let ((widget (widget-create 'editable-field
                               :size 10
                               :secret t
                               value)))
    (puthash id widget widget-map)))

(defun 1password--create-string-widget (id label value widget-map &optional multiline)
  "Create a string widget. If MULTILINE is non-nil, create a notes field."
  (let* ((args (if multiline
                   (list 'editable-field value)
                 (list 'editable-field :size 40 value)))
         (widget (apply #'widget-create args)))
    (puthash id widget widget-map)))

(defun 1password--save-button (widget-map)
  "Add a Save button to the form."
  (widget-insert "\n")
  (widget-create 'push-button
                 :notify (lambda (widget &rest ignore)
                           (let ((form-data (1password--extract-values widget-map)))
                             (message form-data)))
                 "Save and Close"))

(defun 1password--section-header (label)
  "Insert a formatted section header into the buffer."
  (unless (string= label "General") ; Don't print a header for the default group
    (widget-insert "\n")
    (widget-insert (propertize (format "%s" label)
                               'face '1password-section-header-face))
    (widget-insert "\n")))

(defun 1password--widget-dispatcher (field widget-map)
  "Dispatch to the correct widget creation function based on FIELD-DEF."
  (let ((id  (plist-get field :id))
        (type  (plist-get field :type))
        (label  (plist-get field :label))
        (value  (plist-get field :value)))
    (when label
      (widget-insert (format "%-25s: " label)))
    (pcase type
      ("STRING"
       (1password--create-string-widget id label value widget-map))
      ("CONCEALED"
       (1password--create-concealed-widget id label value widget-map))
      ("URL"
       (1password--create-string-widget id label value widget-map))
      ("ADDRESS"
       (1password--create-string-widget id label value widget-map 't))
      ("PHONE"
       (1password--create-string-widget id label value widget-map))
      ;; TODO
      ;; Handle Date
      ;;... other type clauses...
      (_ (message "Warning: Unknown widget type '%s' for id '%s'" type id)))
    (widget-insert "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Form
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun 1password--create-form (json-string)
  "Create and display a dynamic form based on JSON-STRING."
  (interactive "sEnter JSON form definition: ")
  (let* ((record (parse-json-to-plist json-string))
         (form-buffer (get-buffer-create (format "*1password: %s*" (plist-get record :title))))
         (widget-map (make-hash-table :test 'equal))) ; Our ID-to-widget map
    (with-current-buffer form-buffer
      (let ((inhibit-read-only t))
        (widget-minor-mode -1)
        (erase-buffer)
        (widget-setup))
      (widget-minor-mode 1)

      ;; Process data and render the form
      (let* ((parsed-fields (plist-get record :fields))
             (grouped-fields (1password--fields-by-section parsed-fields))
             (sections (plist-get record :sections)))
        (dolist (section sections)
          (let* ((section-id (plist-get section :id))
                 (section-label (plist-get section :label))
                 (fields (assoc section-id grouped-fields)))

            (1password--section-header section-label)
            (dolist (field fields)
              (1password--widget-dispatcher field widget-map))))

        (1password--save-button widget-map)
        (widget-setup)
        (goto-char (point-min)))
      ;;(1password-edit-item-mode))
      (switch-to-buffer form-buffer))))
;;(1password-edit-item-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major Mode
;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defvar 1password-edit-item-mode-keymap
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "C-c C-c") '1password--create-form)
;;     (define-key map (kbd "C-c C-k") 'kill-buffer)
;;     (define-key map (kbd "C-c C-s") '1password--save-button)
;;     (define-key map (kbd "q") 'kill-current-buffer)
;;     map)
;;   "Keymap for `1password-edit-item-mode'.")

;; (define-derived-mode 1password-edit-item-mode special-mode "1Password Edit Item"
;;   "Major mode for editing 1Password items in a dynamic form."
;;   :syntax-table nil
;;   (use-local-map 1password-edit-item-mode-keymap)
;;   (setq buffer-read-only t)
;;   (setq truncate-lines t))

(1password--create-form sample-fields)
