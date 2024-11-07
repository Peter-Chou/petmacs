;;; flymake-mypy.el --- mypy support for flymake -*- lexical-binding: t -*-

;; Author: jason <jason@zzq.org>
;; Created: 26 Sep 2022
;; Version: 0.3.1

;;; Commentary:

;; This module provides support for the mypy static type checker using flymake.

;; Usage:
;;   (require 'flymake-mypy)
;;   (add-hook 'python-mode-hook #'flymake-mypy-enable)

;; Changes:
;;   0.3.1
;;     - fixes issue with (project-current)'s shape changing which prevents
;;       flymake-mypy from starting
;;   0.3.0
;;     - uses mypy's --show-error-end for building more accurate error ranges
;;       (Requires mypy >= 0.981)
;;     - fixes issues with mypy errors causing minor freezing while editing
;;   0.2.0
;;     - use async processing
;;   0.1.0
;;     - initial release

;;; Code:
(require 'project) ;; for project support

(defvar flymake-mypy-executable "python -mmypy"
  "The mypy executable to use for syntax checking.")

(defvar flymake-mypy-output-pattern "^\\(.*\\.py\\):\\([0-9]+?\\):\\([0-9]+?\\):\\([0-9]+?\\):\\([0-9]+?\\): \\(.*?\\): \\(.*\\)$"
  "The regex to use for parsing mypy output")

(defvar-local flymake-mypy--proc nil)

(defun flymake-mypy-enable ()
  "Enable the Mypy checker for Flymake."
  (interactive)
  (add-hook 'flymake-diagnostic-functions 'flymake-mypy--run nil t))

(defun flymake-mypy-disable ()
  "Disable the Mypy checker for Flymake."
  (interactive)
  (remove-hook 'flymake-diagnostic-functions 'flymake-mypy--run nil t))

(defun flymake-mypy--get-position (buffer line column)
  "Calculate position for the given LINE and COLUMN."
  (interactive)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line))  ;; starting from line 1
      (move-to-column column)
      (point))))

(defun flymake-mypy--run (report-fn &rest _args)
  ;; Not having a python interpreter is a serious problem which should cause
  ;; the backend to disable itself, so an error is signaled.
  (unless (executable-find "python")
    (error "Cannot find a suitable python executable for flymake-mypy"))
  (let ((source-buffer (current-buffer)))
    (save-restriction
      (widen)
      (let* ((temp-file (concat (make-temp-file "flymake-mypy") ".py"))
             (default-directory (car (last (project-current)))))
	(write-region (point-min) (point-max) temp-file nil 'quiet)

	(setq flymake-mypy--proc
	      (make-process
	       :name "flymake-mypy"
	       :noquery t
	       :connection-type 'pipe
	       :buffer (generate-new-buffer "*flymake-mypy-output*")
	       :command (mapcar (lambda (x) (shell-quote-argument x))
	       			(flatten-list (list (split-string flymake-mypy-executable " ")
						    "--show-column-numbers"
                                                    "--show-error-end"
						    "--show-absolute-path"
						    "--shadow-file"
						    (buffer-file-name source-buffer)
						    temp-file
						    (buffer-file-name source-buffer))))
	       :sentinel
	       (lambda (proc _event)
		 (when (memq (process-status proc) '(exit signal))
		   (unwind-protect
		       ;; If the buffer local var for this process matches proceed
		       (if (with-current-buffer source-buffer (eq proc flymake-mypy--proc))
			   (with-current-buffer (process-buffer proc)
			     (goto-char (point-min))
			     (cl-loop
			      while (search-forward-regexp flymake-mypy-output-pattern nil t)
			      for line = (match-string 0)
			      for filename = (match-string 1)
			      for line-num = (string-to-number (match-string 2))
			      ;; mypy column numbers are off by 1
			      for col-num = (1- (string-to-number (match-string 3)))
                              for end-line-num = (string-to-number (match-string 4))
                              for end-col-num = (string-to-number (match-string 5))
			      for error-level = (match-string 6)
			      for description  = (format "Mypy (%s): (%s)" error-level (match-string 7))
			      for flymake-err-type = (cond ((string-equal error-level "error") :error)
							   ((string-equal error-level "warning") :warning)
							   (t :note))
			      collect
			      ;; for some reason mypy will sometimes randomly include messages
			      ;; for files not asked for
			      (if (string-equal (buffer-file-name source-buffer) filename)
				  (progn
				    (let* ((beg-region (flymake-mypy--get-position source-buffer line-num col-num))
					   (end-region (flymake-mypy--get-position source-buffer end-line-num end-col-num)))

                                      (flymake-make-diagnostic
				       source-buffer
				       beg-region
				       end-region
				       flymake-err-type
				       description)))
				(flymake-make-diagnostic
				 filename
				 (cons line-num col-num)
				 (cons line-num (1+ col-num))
				 flymake-err-type
				 description))
			      into diags
			      finally (progn
					(if diags
					    (funcall report-fn diags)
					  (funcall report-fn (list))))))
			 (flymake-log :warning "Canceling obsolete check %s" proc))
		     ;; unwind protect is similar to try/finally. this is the finally clause
		     (kill-buffer (process-buffer proc)))))))))))

(provide 'flymake-mypy)
;;; flymake-mypy.el ends here
