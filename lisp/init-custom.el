;; init-custom.el --- Setup custom.el.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defvar  petmacs-proxy "winhost:1080"
  "Set network proxy.")

(defvar petmacs-icon (or (display-graphic-p) (daemonp))
  "Display icons or not.")


(defvar  petmacs-font-size 13.0
  "font size")

(defvar petmacs-lsp-active-modes '(
				                   c-mode
				                   c++-mode
				                   python-mode
				                   java-mode
				                   scala-mode
				                   go-mode
				                   sh-mode
				                   )
  "Primary major modes of the lsp activated layer.")

(defvar petmacs-lsp-client-type 'lsp-mode
  "lsp-mode or lsp-bridge-mode")

(provide 'init-custom)

;;; init-custom.el ends here
