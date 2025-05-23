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
      (apply #'call-process qualifed-executable nil 't  nil (cons "--no-color" (split-string-and-unquote args " ")))
      (special-mode)
      (goto-char (point-min))
      (funcall buffer-reader-fn))))

(cl-defun 1password--execute-async (args
                                    &optional
                                    (process-parse-fn #'json-parse-string)
                                    (buffer-name "*1password*"))
  "Run the 1password executable with `ARGS' and return a promise.

`BUFFER-READER-FN' is a function that will be used to process the
output of the call to the 1Password CLI.  By default, this is
`json-parse-buffer'.

`BUFFER-NAME' is the name of the buffer that will house the
1Password process and that 1Password will dump its output too"
  (let* ((qualifed-executable (executable-find 1password-executable))
         (promise (aio-promise))
         (op-response nil)
         (filter-fn (lambda (_process string)
                      (setq op-response (concat op-response string))))
         (sentinel-fn (lambda (process event)
                        (let ((status (process-status process)))
                          (if (memq status '(exit signal)) ;; Check if process terminated normally or via signal
                              (if (zerop (process-exit-status process))
                                  (condition-case err
                                      (let ((data (funcall process-parse-fn op-response)))
                                        (aio-resolve promise (lambda () data)))
                                    (error (aio-reject promise err))) ;; Reject on parse error
                                (aio-reject promise (format "1Password process failed: %s. Output: %s" event op-response))) ;; Reject on non-zero exit
                            ;; Handle other statuses like 'run', 'stop', etc. if necessary, though unlikely here
                            (aio-reject promise (format "1Password process ended unexpectedly: %s" event)))))))
    (unless qualifed-executable
      (error (format "Unable to find 1Password CLI '%s'" 1password-executable)))
    (make-process :name "1password"
                  :command (append (list qualifed-executable "--no-color") args)
                  :filter filter-fn
                  :sentinel sentinel-fn)
    promise))

;; Local Variables:
;; End:
(provide '1password-lib)
;;; 1password-lib.el ends here
