;; -*- lexical-binding: t no-byte-compile: t -*-

(use-package yaml-mode
  :mode (("\\.ya?ml\\'" . yaml-mode)
         ("Procfile\\'" . yaml-mode)))

(provide 'init-yaml)
