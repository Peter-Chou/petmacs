;; -*- lexical-binding: t no-byte-compile: t -*-

(require 'init-const)
(require 'init-funcs)

(use-package flymake
  :diminish
  :functions my-elisp-flymake-byte-compile
  :hook (prog-mode . flymake-mode)
  :init
  (setq flymake-fringe-indicator-position 'right-fringe
        ;; flymake-no-changes-timeout nil
        ;; flymake-start-on-save-buffer t
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
  :custom-face
  (sideline-flymake-error ((t (:height 0.85 :italic t))))
  (sideline-flymake-warning ((t (:height 0.85 :italic t))))
  (sideline-flymake-success ((t (:height 0.85 :italic t))))
  :init (setq
         ;; sideline-flymake-display-mode 'point
         sideline-flymake-display-mode 'line))

(use-package flymake-ruff
  :demand t
  :preface
  (defun petmacs/setup-flymake-ruff ()
    (setq-local lsp-diagnostics-provider :none)
    (flymake-ruff-load))

  (defun petmacs/eglot-setup-flymake-ruff ()
    (interactive)
    (when (memq major-mode '(python-mode python-ts-mode))
      (flymake-ruff-load)))
  :config
  (add-hook 'eglot-managed-mode-hook 'petmacs/eglot-setup-flymake-ruff)

  (defun petmacs/filter-eglot-diagnostics (diags)
    "Drop all Pyright diagnose from langserver"
    (list (seq-remove (lambda (d)
                        (string-match "Pyright" (flymake-diagnostic-text d)))
                      (car diags))))

  (advice-add 'eglot--report-to-flymake :filter-args #'petmacs/filter-eglot-diagnostics))

(provide 'init-flymake)
