;; -*- lexical-binding: t no-byte-compile: t -*-

(require 'init-const)
(require 'init-funcs)

(use-package flymake
  :diminish
  :functions my-elisp-flymake-byte-compile
  :hook (prog-mode . flymake-mode)
  :init
  (setq flymake-fringe-indicator-position 'right-fringe
        flymake-no-changes-timeout nil
        flymake-start-on-save-buffer t
        )
  :config
  ;; Check elisp with `load-path'
  (defun my-elisp-flymake-byte-compile (fn &rest args)
    "Wrapper for `elisp-flymake-byte-compile'."
    (let ((elisp-flymake-byte-compile-load-path
           (append elisp-flymake-byte-compile-load-path load-path)))
      (apply fn args)))
  (advice-add 'elisp-flymake-byte-compile :around #'my-elisp-flymake-byte-compile))


(use-package sideline-flymake
  :diminish sideline-mode
  :hook (flymake-mode . sideline-mode)
  :init (setq sideline-flymake-display-mode 'point
              sideline-backends-right '(sideline-flymake)))

(use-package flymake-ruff
  :demand t
  :preface
  (defun petmacs/setup-flymake-ruff ()
    (setq-local lsp-diagnostics-provider :none)
    (flymake-ruff-load))
  :config
  (pcase petmacs-lsp-mode-impl
    ('lsp-mode
     (add-hook 'python-mode-hook #'petmacs/setup-flymake-ruff)
     (add-hook 'python-ts-mode-hook #'petmacs/setup-flymake-ruff))
    ('eglot-mode
     (defun petmacs/filter-eglot-diagnostics (diags)
       "Drop Pyright 'variable not accessed' notes from DIAGS."
       (list (seq-remove (lambda (d)
                           (and (eq (flymake-diagnostic-type d) 'eglot-note)
                                (s-starts-with? "Pyright:" (flymake-diagnostic-text d))
                                (s-ends-with? "is not accessed" (flymake-diagnostic-text d))))
                         (car diags))))
     (advice-add 'eglot--report-to-flymake :filter-args #'petmacs/filter-eglot-diagnostics)
     (add-hook 'python-mode-hook 'flymake-ruff-load)
     (add-hook 'python-ts-mode-hook 'flymake-ruff-load))))

(provide 'init-flymake)
