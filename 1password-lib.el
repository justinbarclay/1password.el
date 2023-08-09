;;; 1password-lib.el --- 1password functions for interacting with the item subcommand -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'aio)
(require 'cl-lib)
(require 'json)

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

(cl-defun 1password--execute-in-buffer-async (args
                                              &optional
                                              (buffer-reader-fn #'json-parse-buffer)
                                              (buffer-name "*1password*"))
  "Run the 1password executable with `ARGS' and return a promise.

`BUFFER-READER-FN' is a function that will be used to process the
output of the call to the 1Password CLI.  By default, this is
`json-parse-buffer'.

`BUFFER-NAME' is the name of the buffer that will house the
1Password process and that 1Password will dump its output too"
  (with-current-buffer (get-buffer-create buffer-name)
    (let* ((qualifed-executable (executable-find 1password-executable))
           (promise (aio-promise))
           (sentinel-fn (lambda (process event)
                          (let ((data (with-current-buffer (process-buffer process)
                                                   (special-mode)
                                                   (goto-char (point-min))
                                                   (funcall buffer-reader-fn))))
                            (aio-resolve promise (lambda () data))))))
      (unless qualifed-executable
        (error (format "Unable to find 1Password CLI '%s'" 1password-executable)))
      (read-only-mode -1)
      (erase-buffer)
      (make-process :name "1password"
                    :buffer buffer-name
                    :command (cons 1password-executable (split-string args " "))
                    :sentinel sentinel-fn)
      promise)))

;; Local Variables:
;; End:
(provide '1password-lib)
;;; 1password-lib.el ends here
