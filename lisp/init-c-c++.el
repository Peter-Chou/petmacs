;; -*- lexical-binding: t no-byte-compile: t -*-

(use-package cc-mode
  :init
  (add-to-list 'auto-mode-alist
	           `("\\.h\\'" . c++-mode))
  ;; (setq lsp-clients-clangd-args
  ;;       '("-j=6" "-log=verbose" "-background-index"
  ;;         ;; "-cross-file-rename"
  ;;         ))
  :config
  (require 'compile)
  (add-to-list 'auto-mode-alist '("\.cu$" . c++-mode)))

(use-package smart-semicolon
  :hook (((c-mode-common java-ts-mode) . smart-semicolon-mode)))

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode)))

(provide 'init-c-c++)
