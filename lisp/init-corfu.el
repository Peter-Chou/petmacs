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
  (when (> (frame-pixel-width) 3000) (custom-set-faces '(corfu-default ((t (:height 1.3))))))
  (global-corfu-mode)
  :config
  ;; 默认用这三个补全后端
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (with-eval-after-load 'lsp-mode
    (setq lsp-completion-provider :none) ;; we use Corfu!

    (defun petmacs/lsp-mode-setup-completion ()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless))) ;; Configure orderless

    (add-hook 'lsp-completion-mode-hook #'petmacs/lsp-mode-setup-completion)))

;; ;; Use dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

;; ;; Add extensions

(provide 'init-corfu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-corfu.el ends here
