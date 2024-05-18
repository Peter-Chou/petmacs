;; -*- lexical-binding: t no-byte-compile: t -*-

(use-package cc-mode
  :init
  (add-to-list 'auto-mode-alist
	           `("\\.h\\'" . c++-mode))
  :config
  (require 'compile)
  (add-to-list 'auto-mode-alist '("\.cu$" . c++-mode)))

(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode)))

(provide 'init-c-c++)
