;; -*- lexical-binding: t no-byte-compile: t -*-

(eval-when-compile
  (require 'init-const)
  (require 'init-funcs))

(use-package flymake
  :diminish
  :functions my-elisp-flymake-byte-compile
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-project-diagnostics-mode-map
         ("C-n" . evil-next-visual-line)
         ("C-p" . evil-previous-visual-line)
         ("M-n" . evil-next-visual-line)
         ("M-p" . evil-previous-visual-line))
  :init
  (setq flymake-fringe-indicator-position 'right-fringe
        flymake-margin-indicator-position 'right-margin
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

(defun petmacs/filter-eglot-basedpyright-diagnostics (diags)
  "Drop all basedpyright diagnose from langserver"
  (list (seq-remove (lambda (d)
                      (string-match "basedpyright" (flymake-diagnostic-text d))
                      ;; (s-starts-with? "basedpyright" (flymake-diagnostic-text d))
                      )
                    (car diags))))
;; (advice-add 'eglot--report-to-flymake :filter-args #'petmacs/filter-eglot-basedpyright-diagnostics)

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
  (add-hook 'eglot-managed-mode-hook 'petmacs/eglot-setup-flymake-ruff))

;; (use-package flymake-popon
;;   :diminish
;;   :custom-face
;;   (flymake-popon-posframe-border ((t :foreground ,(face-background 'region))))
;;   :hook (flymake-mode . flymake-popon-mode)
;;   :init (setq flymake-popon-width 70
;;               flymake-popon-delay 1
;;               flymake-popon-posframe-border-width 1
;;               flymake-popon-method (if (childframe-workable-p) 'posframe 'popon)))

(provide 'init-flymake)
