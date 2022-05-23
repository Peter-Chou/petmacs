;; init-corfu.el --- Better default configurations.	-*- lexical-binding: t -*-

(require 'init-const)
(require 'init-funcs)

(use-package corfu
  :bind (:map corfu-map
         ("C-M-m" . corfu-move-to-minibuffer))
  :init
  (setq corfu-cycle t
        corfu-auto t
        corfu-quit-at-boundary t
        corfu-quit-no-match t
        corfu-preview-current nil
        ;; corfu-preselect-first t
        corfu-auto-delay 0.2
        corfu-auto-prefix 1
        )
  (global-corfu-mode)
  :config
  (with-eval-after-load 'lsp-mode
    (setq lsp-completion-provider :none) ;; we use Corfu!

    (defun petmacs/lsp-mode-setup-completion ()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless))) ;; Configure orderless

    (add-hook 'lsp-completion-mode-hook #'petmacs/lsp-mode-setup-completion)))

(use-package kind-all-the-icons
  :ensure nil
  :init
  (require 'kind-all-the-icons)
  (add-to-list 'corfu-margin-formatters #'kind-all-the-icons-margin-formatter))

;; ;; Use dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

;; ;; Add extensions

(provide 'init-corfu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-corfu.el ends here
