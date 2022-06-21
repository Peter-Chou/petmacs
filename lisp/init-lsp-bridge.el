;;	-*- lexical-binding: t -*-

(use-package corfu-doc)

;;; pip install epc
(use-package lsp-bridge
  :load-path (lambda () (expand-file-name "site-lisp/lsp-bridge" user-emacs-directory))
  :init (require 'lsp-bridge)
  :config
  ;; (setq-local evil-goto-definition-functions '(lsp-bridge-jump))

  (define-key evil-motion-state-map "gR" #'lsp-bridge-rename)
  (define-key evil-motion-state-map "gr" #'lsp-bridge-find-references)
  (define-key evil-normal-state-map "gi" #'lsp-bridge-find-impl)
  ;; (define-key evil-motion-state-map "gd" #'lsp-bridge-jump)
  (define-key evil-motion-state-map "gs" #'lsp-bridge-restart-process)
  (define-key evil-normal-state-map "gh" #'lsp-bridge-lookup-documentation)

  (global-lsp-bridge-mode)
  )


(provide 'init-lsp-bridge)
