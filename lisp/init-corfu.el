;; init-corfu.el --- Better default configurations.	-*- lexical-binding: t -*-

(require 'init-const)
(require 'init-funcs)

;;; corfu related
(when (not (display-graphic-p))
  (use-package company
    :init
    (setq company-minimum-prefix-length 1)
    (setq company-idle-delay 0)
    (global-company-mode t))

  (use-package company-flx
    :after (company)
    :init
    (company-flx-mode 1))
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-j") #'company-select-next)
    (define-key company-active-map (kbd "C-k") #'company-select-previous)))

(defun nasy/orderless-dispatch-flex-first (_pattern index _total)
  "orderless-flex for corfu."
  (and (eq index 0) 'orderless-flex))

(defun nasy/setup-corfu ()
  "Setup corfu."
  (setq-local orderless-matching-styles '(orderless-flex)
              orderless-style-dispatchers nil)
  (add-hook 'orderless-style-dispatchers #'nasy/orderless-dispatch-flex-first nil 'local))

;; use corfu instead
(when (display-graphic-p)
  (use-package corfu
    :init
    (setq ;; corfu-cycle t
          corfu-auto t
          ;; corfu-quit-at-boundary t
          ;; corfu-quit-no-match t
          corfu-preview-current nil
          ;; corfu-preselect-first t
          corfu-auto-delay 0.2
          corfu-auto-prefix 1)
    (global-corfu-mode))

  ;; ;; elisp requires emacs28
  ;; (use-package kind-icon
  ;;   :after corfu
  ;;   :custom (kind-icon-default-face 'corfu-default)
  ;;   :config
  ;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  ;;   (add-hook 'my-completion-ui-mode-hook
  ;;  	          (lambda ()
  ;;  	            (setq completion-in-region-function
  ;;  		              (kind-icon-enhance-completion
  ;;  		               completion-in-region-function)))))

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

  (use-package cape
    :bind (("C-M-o"   . cape-file))
    :init
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)))


;; ;; Add extensions

(provide 'init-corfu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-corfu.el ends here
