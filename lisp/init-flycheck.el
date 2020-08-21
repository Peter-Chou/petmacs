;; init-flycheck.el --- Setup flycheck.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

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
  (flycheck-redefine-standard-error-levels "⏴" 'flycheck-fringe-bitmap-arrow)

  ;; Display Flycheck errors in GUI tooltips
  (if (display-graphic-p)
      (if emacs/>=26p
          (use-package flycheck-posframe
            :custom-face (flycheck-posframe-border-face ((t (:inherit font-lock-comment-face))))
            :hook (flycheck-mode . flycheck-posframe-mode)
            :init (setq flycheck-posframe-border-width 1
                        flycheck-posframe-inhibit-functions
                        '((lambda (&rest _) (bound-and-true-p company-backend)))))
        (use-package flycheck-pos-tip
          :defines flycheck-pos-tip-timeout
          :hook (global-flycheck-mode . flycheck-pos-tip-mode)
          :config (setq flycheck-pos-tip-timeout 30)))
    (use-package flycheck-popup-tip
      :hook (flycheck-mode . flycheck-popup-tip-mode))))

(provide 'init-flycheck)

;;; init-flycheck.el ends here
