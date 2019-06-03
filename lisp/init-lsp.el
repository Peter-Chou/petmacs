;; init-lsp.el --- Setup lsp.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package lsp-mode
  ;; :pin melpa-stable
  :commands lsp
  :init
  (setq lsp-auto-guess-root t)
  (setq lsp-prefer-flymake nil)
  :config
  (progn
    (require 'lsp-clients)))

(use-package lsp-ui
  ;; :pin melpa-stable
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :hook (lsp-ui-imenu-mode . (lambda ()
			       (display-line-numbers-mode -1)
			       (hl-line-mode -1)))
  :init
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-use-webkit t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-position 'top)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-imenu-enable t)
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-ignore-duplicate t)
  (evil-define-key 'normal lsp-ui-imenu-mode-map (kbd "q") 'lsp-ui-imenu--kill)
  (evil-define-key 'normal lsp-ui-imenu-mode-map (kbd "J") 'lsp-ui-imenu--next-kind)
  (evil-define-key 'normal lsp-ui-imenu-mode-map (kbd "K") 'lsp-ui-imenu--prev-kind)
  (evil-define-key 'normal lsp-ui-imenu-mode-map (kbd "<return>") 'lsp-ui-imenu--visit)
  (evil-define-key 'normal lsp-ui-imenu-mode-map (kbd "d") 'lsp-ui-imenu--view)
  :config
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))

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

(use-package company-lsp
  ;; :pin melpa-stable
  :init (setq company-lsp-cache-candidates 'auto))

;; `lsp-mode' and `treemacs' integration.
(use-package lsp-treemacs
  :bind (:map lsp-mode-map
                ("M-9" . lsp-treemacs-errors-list)))

(provide 'init-lsp)

;;; init-lsp.el ends here
