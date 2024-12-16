;; -*- lexical-binding: t no-byte-compile: t -*-

(eval-when-compile
  (require 'init-const)
  (require 'init-funcs))

(use-package flycheck
  :diminish
  :commands flycheck-redefine-standard-error-levels
  :hook (after-init . global-flycheck-mode)
  :init (setq flycheck-global-modes
              '(not text-mode outline-mode fundamental-mode lisp-interaction-mode
                    org-mode diff-mode shell-mode eshell-mode term-mode vterm-mode)
              flycheck-emacs-lisp-load-path 'inherit
              flycheck-indication-mode (if (display-graphic-p)
                                           'right-fringe
                                         'right-margin)
              ;; Only check while saving and opening files
              flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  ;; Prettify indication styles
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-arrow
      [16 48 112 240 112 48 16] nil nil 'center))
  (flycheck-redefine-standard-error-levels "⏴" 'flycheck-fringe-bitmap-arrow))

(use-package consult-flycheck
  :after (consult flycheck))

(use-package sideline-flycheck
  :custom-face
  (sideline-flycheck-error ((t (:height 0.85 :italic t))))
  (sideline-flycheck-warning ((t (:height 0.85 :italic t))))
  (sideline-flycheck-success ((t (:height 0.85 :italic t))))
  :hook (flycheck-mode . sideline-flycheck-setup))

(add-hook 'eglot-managed-mode-hook (lambda () (flymake-mode -1)))

(provide 'init-flycheck)
