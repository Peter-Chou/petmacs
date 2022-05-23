;; init-corfu.el --- Better default configurations.	-*- lexical-binding: t -*-

(require 'init-const)
(require 'init-funcs)

(use-package corfu
  :preface
  (defun petmacs/orderless-dispatch-flex-first (_pattern index _total)
    "orderless-flex for corfu."
    (and (eq index 0) 'orderless-flex))
  (defun petmacs/setup-corfu ()
    "Setup corfu."
    (setq-local orderless-matching-styles '(orderless-flex)
                orderless-style-dispatchers nil)
    (add-hook 'orderless-style-dispatchers #'petmacs/orderless-dispatch-flex-first nil 'local))
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (toggle-chinese-search)
      (apply #'consult-completion-in-region completion-in-region--data)))
  :hook (prog-mode . petmacs/setup-corfu)
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
    (defun petmacs/lsp-mode-setup-completion ()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(flex))) ;; Configure flex
    (setq lsp-completion-provider :none) ;; we use Corfu!
    (add-hook 'lsp-completion-mode-hook #'petmacs/lsp-mode-setup-completion)))

;; elisp requires emacs28
;; (use-package kind-icon
;;   :after corfu
;;   :custom (kind-icon-default-face 'corfu-default)
;;   :config
;;   ;; (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
;;   (add-hook 'my-completion-ui-mode-hook
;;    	        (lambda ()
;;    	          (setq completion-in-region-function
;;    		            (kind-icon-enhance-completion
;;    		             completion-in-region-function)))))

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
