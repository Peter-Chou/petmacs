;; init-lsp.el --- Setup lsp.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package lsp-mode
  ;; :pin melpa-stable
  :commands lsp
  :diminish lsp-mode
  :bind (:map lsp-mode-map
	      ("C-c C-d" . lsp-describe-thing-at-point))
  :init
  (setq lsp-auto-guess-root t		;; Detect project root
	lsp-prefer-flymake nil		;; Use lsp-ui and flycheck
	flymake-fringe-indicator-position 'right-fringe)
  :config
  (progn
    (require 'lsp-clients)))

(use-package lsp-ui
  ;; :pin melpa-stable
  :commands lsp-ui-doc-hide
  :custom-face (lsp-ui-doc-background ((t (:background ,(face-background 'tooltip)))))
  :hook (after-load-theme . (lambda ()
			      (set-face-attribute 'lsp-ui-doc-background nil
						  :background (face-background 'tooltip))))
  :bind (:map lsp-ui-mode-map
	      ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	      ([remap xref-find-references] . lsp-ui-peek-find-references)
	      ("C-c u" . lsp-ui-imenu))
  :hook (lsp-ui-imenu-mode . (lambda ()
			       (display-line-numbers-mode -1)
			       (hl-line-mode -1)))
  :init
  (setq lsp-ui-doc-enable t
	lsp-ui-peek-enable t
	lsp-ui-doc-use-webkit nil
	lsp-ui-doc-include-signature t
	lsp-ui-doc-position 'at-point
	lsp-ui-doc-border (face-foreground 'default)

	lsp-ui-sideline-enable nil
	lsp-ui-sideline-ignore-duplicate t)

  ;; (setq lsp-ui-imenu-enable t)
  ;; (setq lsp-ui-flycheck-enable t)

  (evil-define-key 'normal lsp-ui-imenu-mode-map (kbd "q") 'lsp-ui-imenu--kill)
  (evil-define-key 'normal lsp-ui-imenu-mode-map (kbd "J") 'lsp-ui-imenu--next-kind)
  (evil-define-key 'normal lsp-ui-imenu-mode-map (kbd "K") 'lsp-ui-imenu--prev-kind)
  (evil-define-key 'normal lsp-ui-imenu-mode-map (kbd "<return>") 'lsp-ui-imenu--visit)
  (evil-define-key 'normal lsp-ui-imenu-mode-map (kbd "d") 'lsp-ui-imenu--view)
  :config
  (add-to-list 'lsp-ui-doc-frame-parameters '(left-fringe . 0))

  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; @see https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))

(use-package company-lsp
  ;; :pin melpa-stable
  :init (setq company-lsp-cache-candidates 'auto))

;; Debug
;; (use-package dap-mode
;;   :after lsp-mode
;;   :diminish
;;   :hook ((after-init . dap-mode)
;;          (dap-mode . dap-ui-mode)

;;          (python-mode . (lambda () (require 'dap-python)))
;;          (go-mode . (lambda () (require 'dap-go)))
;;          (java-mode . (lambda () (require 'dap-java)))
;;          ((c-mode c++-mode objc-mode swift) . (lambda () (require 'dap-lldb)))
;;          (php-mode . (lambda () (require 'dap-php)))))

;; `lsp-mode' and `treemacs' integration.
(use-package lsp-treemacs
  :bind (:map lsp-mode-map
	      ("M-9" . lsp-treemacs-errors-list)))

(provide 'init-lsp)

;;; init-lsp.el ends here
