;; -*- lexical-binding: t no-byte-compile: t -*-

(use-package yaml-mode
  :mode (("\\.ya?ml\\'" . yaml-mode)
         ("Procfile\\'" . yaml-mode)))

;; (use-package json-mode)
(use-package toml-mode)

(provide 'init-markup)
