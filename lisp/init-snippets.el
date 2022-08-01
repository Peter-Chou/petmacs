;; -*- lexical-binding: t no-byte-compile: t -*-

(use-package yasnippet
  :diminish yas-minor-mode
  :bind ("C-M-e" . yas-expand)
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets)

(provide 'init-snippets)
