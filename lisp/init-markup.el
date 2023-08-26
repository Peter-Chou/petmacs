;; -*- lexical-binding: t no-byte-compile: t -*-

;; (use-package yaml-mode
;;   :mode (("\\.ya?ml\\'" . yaml-mode)
;;          ("Procfile\\'" . yaml-mode)))

(use-package json-ts-mode
  :ensure nil
  :mode (("\\.jsonl?\\'" . json-ts-mode)))

(use-package yaml-ts-mode
  :ensure nil
  :mode (("\\.ya?ml\\'" . yaml-ts-mode)
         ("Procfile\\'" . yaml-ts-mode)))

(use-package nxml-mode
  :ensure nil
  :config
  (define-key nxml-mode-map (kbd "gc") #'evilnc-comment-or-uncomment-lines))

;; (use-package json-mode)
(use-package toml-mode)

(provide 'init-markup)
