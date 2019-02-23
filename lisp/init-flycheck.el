;; init-flycheck.el --- Setup flycheck.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package flycheck
  :diminish flycheck-mode
  :hook (after-init . global-flycheck-mode)
  :init
  (evil-define-key 'normal flycheck-error-list-mode-map (kbd "RET") 'flycheck-error-list-goto-error)
  (evil-define-key 'normal flycheck-error-list-mode-map (kbd "j") 'flycheck-error-list-next-error)
  (evil-define-key 'normal flycheck-error-list-mode-map (kbd "k") 'flycheck-error-list-previous-error)
  :config
  (setq flycheck-indication-mode 'right-fringe)
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Only check while saving and opening files
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  ;; Display Flycheck errors in GUI tooltips
  (if (display-graphic-p)
      (if emacs/>=26p
          (use-package flycheck-posframe
            :hook (flycheck-mode . (lambda ()
                                     (unless (and (bound-and-true-p lsp-mode)
                                                  (bound-and-true-p lsp-ui-flycheck-enable))
                                       (flycheck-posframe-mode 1)))))
        (use-package flycheck-pos-tip
          :defines flycheck-pos-tip-timeout
          :hook (global-flycheck-mode . flycheck-pos-tip-mode)
          :config (setq flycheck-pos-tip-timeout 30)))
    (use-package flycheck-popup-tip
      :hook (flycheck-mode . (lambda ()
                               (unless (and (bound-and-true-p lsp-mode)
                                            (bound-and-true-p lsp-ui-flycheck-enable))
                                 (flycheck-popup-tip-mode 1))))))

  ;; Jump to and fix syntax errors via `avy'
  (use-package avy-flycheck
    :hook (global-flycheck-mode . avy-flycheck-setup)))

(provide 'init-flycheck)

;;; init-flycheck.el ends here