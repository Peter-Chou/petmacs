;; init-c-c++.el --- C/C++ configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; C/C++ configurations.
;;

;;; Code:

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-c-c++.el ends here
