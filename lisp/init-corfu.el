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
    (setq corfu-cycle t)
    (setq corfu-auto t)
    ;; (setq corfu-quit-at-boundary t)
    ;; (setq corfu-quit-no-match t)
    ;; (setq corfu-preview-current nil)
    ;; (setq corfu-min-width 80)
    ;; (setq corfu-max-width 100)
    ;; (setq corfu-auto-delay 0.2)
    ;; (setq corfu-auto-prefix 1)
    (global-corfu-mode)
    ;; :hook (prog-mode . nasy/setup-corfu)
    :config
    ;; (define-key corfu-map (kbd "C-j") 'corfu-next)
    ;; (define-key corfu-map (kbd "C-k") 'corfu-previous)
    )

  ;; (use-package corfu-doc
  ;;   :hook (corfu-mode . corfu-doc-mode))

  ;; ;; elisp requires emacs28
  ;; (use-package kind-icon
  ;;   :after corfu
  ;;   :custom ;;   (kind-icon-default-face 'corfu-default)
  ;;   :config
  ;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

  ;; ;; Use dabbrev with Corfu!
  ;; (use-package dabbrev
  ;;   ;; Swap M-/ and C-M-/
  ;;   :bind (("M-/" . dabbrev-completion)
  ;;          ("C-M-/" . dabbrev-expand)))

  ;; ;; A few more useful configurations...
  ;; (use-package emacs
  ;;   :init
  ;;   ;; TAB cycle if there are only few candidates
  ;;   (setq completion-cycle-threshold 3)
  ;;   ;; Enable recursive minibuffers
  ;;   ;; (setq enable-recursive-minibuffers t)

  ;;   ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;;   ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;;   ;; (setq read-extended-command-predicate
  ;;   ;;       #'command-completion-default-include-p)

  ;;   ;; Enable indentation+completion using the TAB key.
  ;;   ;; `completion-at-point' is often bound to M-TAB.
  ;;   (setq tab-always-indent 'complete))
  ;; ;; Add extensions
  ;; (use-package cape
  ;;   :init
  ;;   ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;;   (add-to-list 'completion-at-point-functions #'cape-file)
  ;;   (add-to-list 'completion-at-point-functions #'cape-tex)
  ;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;;   (setq cape-dabbrev-check-other-buffers nil)
  ;;   (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;   )
  )

(provide 'init-corfu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-corfu.el ends here
