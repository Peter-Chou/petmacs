;; -*- lexical-binding: t no-byte-compile: t -*-

(use-package markdown-mode
  :hook ((markdown-mode . auto-fill-mode))
  :mode (("README\\.md\\'" . gfm-mode))
  :init
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-uppercase-checkbox t
        markdown-fontify-code-blocks-natively t)
  )

;; Table of contents
(use-package markdown-toc
  :bind (:map markdown-mode-command-map
         ("r" . markdown-toc-generate-or-refresh-toc)))

(provide 'init-markdown)
