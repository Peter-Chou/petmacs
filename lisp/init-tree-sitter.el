;; -*- lexical-binding: t no-byte-compile: t -*-

;; M-x treesit-auto-install-all
(use-package treesit-auto
  :demand t
  :init
  (setq treesit-auto-install 'prompt
        treesit-font-lock-level 4)
  :config
  (global-treesit-auto-mode))

(provide 'init-tree-sitter)
