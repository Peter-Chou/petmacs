;; -*- lexical-binding: t no-byte-compile: t -*-

(use-package yaml-mode
  :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
         ("Procfile\\'" . yaml-mode)))

(provide 'init-yaml)
