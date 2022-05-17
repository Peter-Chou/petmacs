;; init-snippets.el --- Better default configurations.	-*- lexical-binding: t -*-

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'init-snippets)
