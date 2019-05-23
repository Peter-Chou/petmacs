;; init-elisp.el --- Setup Emacs lisp IDE.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Emacs lisp mode
(use-package elisp-mode
  :ensure nil
  :bind (:map emacs-lisp-mode-map
              ("C-c C-x" . ielm)
              ("C-c C-c" . eval-defun)
              ("C-c C-b" . eval-buffer))
  :config
  (if (boundp 'elisp-flymake-byte-compile-load-path)
      (add-to-list 'elisp-flymake-byte-compile-load-path load-path))

  ;; Add remove buttons for advices
  (add-hook 'help-mode-hook 'cursor-sensor-mode)

  (defun function-advices (function)
    "Return FUNCTION's advices."
    (let ((function-def (advice--symbol-function function))
          (ad-functions '()))
      (while (advice--p function-def)
        (setq ad-functions (append `(,(advice--car function-def)) ad-functions))
        (setq function-def (advice--cdr function-def)))
      ad-functions))

  (define-advice describe-function-1 (:after (function) advice-remove-button)
    "Add a button to remove advice."
    (when (get-buffer "*Help*")
      (with-current-buffer "*Help*"
        (save-excursion
          (goto-char (point-min))
          (let ((ad-index 0)
                (ad-list (reverse (function-advices function))))
            (while (re-search-forward "^:[-a-z]+ advice: \\(.+\\)$" nil t)
              (let* ((name (string-trim (match-string 1) "'" "'"))
                     (advice (or (intern-soft name) (nth ad-index ad-list))))
                (when (and advice (functionp advice))
                  (let ((inhibit-read-only t))
                    (insert "\t")
                    (insert-text-button
                     "[Remove]"
                     'cursor-sensor-functions `((lambda (&rest _) (message "%s" ',advice)))
                     'help-echo (format "%s" advice)
                     'action
                     ;; In case lexical-binding is off
                     `(lambda (_)
                        (when (yes-or-no-p (format "Remove %s ? " ',advice))
                          (message "Removing %s of advice from %s" ',function ',advice)
                          (advice-remove ',function ',advice)
                          (revert-buffer nil t)))
                     'follow-link t))))
              (setq ad-index (1+ ad-index))))))))
  ;; Remove hook
  (defun remove-hook-at-point ()
    "Remove the hook at the point in the *Help* buffer."
    (interactive)
    (unless (or (eq major-mode 'help-mode)
                (string= (buffer-name) "*Help*"))
      (error "Only for help-mode"))
    (let ((orig-point (point)))
      (save-excursion
        (when-let*
            ((hook (progn (goto-char (point-min)) (symbol-at-point)))
             (func (when (and
                          (or (re-search-forward
                               (format "%s value is[\s\n]" hook) nil t)
                              (re-search-forward
                               (format "^Value:[\s\n]") nil t)
                              (re-search-forward
                               (format "global value is [\n]") nil t))
                          (sexp-at-point))
                     (end-of-sexp)
                     (backward-char 1)
                     (catch 'break
                       (while t
                         (condition-case err
                             (backward-sexp)
                           (scan-error (throw 'break nil)))
                         (let ((bounds (bounds-of-thing-at-point 'sexp)))
                           (when (< (car bounds) orig-point (cdr bounds))
                             (throw 'break (sexp-at-point)))))))))
          (when (yes-or-no-p (format "Remove %s from %s? " func hook))
            (remove-hook hook func)
            (revert-buffer nil t))))))
  (bind-key "C-c d" #'remove-hook-at-point help-mode-map))

;; Show function arglist or variable docstring
;; `global-eldoc-mode' is enabled by default.
(use-package eldoc
  :ensure nil
  :diminish eldoc-mode)

;; Semantic code search for emacs lisp
(use-package elisp-refs)

;; A better *Help* buffer
(use-package helpful
  :defines ivy-initial-inputs-alist
  :bind (("C-c C-d" . helpful-at-point)))

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(provide 'init-elisp)

;;; init-elisp.el ends here
