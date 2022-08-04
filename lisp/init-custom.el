;; -*- lexical-binding: t no-byte-compile: t -*-

(defvar petmacs-icon (or (display-graphic-p) (daemonp))
  "Display icons or not.")

(defvar petmacs-font
  ;; "Monego"
  "Monego Ligatures"
  ;; "MonegoLigatures Nerd Font"
  "font")

(defvar  petmacs-chinese-font
  "等距更纱黑体 SC"
  "chinese font")

(defvar  petmacs-font-size 14.0
  "font size")

(defvar petmacs-enable-ligatures t
  "enable ligatures")

(defvar  petmacs-default-theme
  ;; 'modus-operandi ;; light theme
  'modus-vivendi  ;; dark theme
  ;; 'doom-ayu-dark
  "default theme")

(defvar petmacs-modeline-style
  ;; 'awesome-tray
  'doom-modeline
  "awesome-tray or doom-modeline. doom-modeline is default in TUI.")

(defvar petmacs-enable-mini-frame nil
  "enable mini frame feature")

(defvar petmacs-enable-display-line-numbers nil
  "enable display-line-numbers mode")

(defvar petmacs-lsp-client-mode
  'lsp-mode
  ;; 'lsp-bridge-mode
  "lsp-mode or lsp-bridge-mode")

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

(defvar petmacs-lsp-format-on-save-ignore-modes
  '(c-mode c++-mode python-mode markdown-mode))

(defvar  petmacs-proxy "winhost:1080"
  "Set network proxy.")

(defvar petmacs-socks-proxy "127.0.0.1:1086"
  "Set SOCKS proxy.")

(provide 'init-custom)
