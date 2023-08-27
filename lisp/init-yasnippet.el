;; -*- lexical-binding: t no-byte-compile: t -*-

(use-package yasnippet
  :diminish yas-minor-mode
  :bind ("M-e" . yas-expand)
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets)

(use-package yasnippet-capf
  :init
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))


(provide 'init-yasnippet)
