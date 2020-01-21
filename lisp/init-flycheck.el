;; init-flycheck.el --- Setup flycheck.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package flycheck
  :diminish
  :hook (after-init . global-flycheck-mode)
  :init
  (evil-define-key 'normal flycheck-error-list-mode-map (kbd "RET") 'flycheck-error-list-goto-error)
  (evil-define-key 'normal flycheck-error-list-mode-map (kbd "j") 'flycheck-error-list-next-error)
  (evil-define-key 'normal flycheck-error-list-mode-map (kbd "k") 'flycheck-error-list-previous-error)
  :config
  (setq flycheck-global-modes
        '(not org-mode text-mode outline-mode fundamental-mode
              shell-mode eshell-mode term-mode vterm-mode)
        flycheck-emacs-lisp-load-path 'inherit
        ;; Only check while saving and opening files
        flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-indication-mode 'right-fringe)

  ;; Prettify fringe style
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))

  ;; Display Flycheck errors in GUI tooltips
  (if (display-graphic-p)
      (if emacs/>=26p
          (use-package flycheck-posframe
            :custom-face (flycheck-posframe-border-face ((t (:inherit default))))
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
