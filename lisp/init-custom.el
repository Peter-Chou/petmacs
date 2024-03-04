;; -*- lexical-binding: t no-byte-compile: t -*-

(eval-when-compile
  (require 'package))

(defvar petmacs-icon t
  "Display icons or not.")

(defvar petmacs-treesit t
  "Enable tree-sitter or not. Native tree-sitter is introduced in 29.")

(defvar petmacs-completion-style 'minibuffer
  "minibuffer / childframe")

(defvar petmacs-favor-color "#F37022"
  "petmacs favor color")

(defvar petmacs-font
  ;; "Monego"
  "Monego Ligatures"
  ;; "Maple Mono"
  "font")

(defvar  petmacs-chinese-font
  "Maple Mono SC NF"
  ;; "等距更纱黑体 SC"
  "chinese font")

(defvar  petmacs-font-size 14.0
  "font size")

(defvar petmacs-enable-ligatures t
  "enable ligatures")

(defvar petmacs-quelpa-checkout-melpa nil
  "update melpa, disable it if the network is unstable.")

(defvar  petmacs-day-night-themes
  ;; '(("8:00" . spacemacs-light)
  ;;   ("20:00"  . doom-dracula))
  '(("8:00" . doom-shades-of-purple)
    ("20:00"  . doom-shades-of-purple))
  "day night theme")

(defvar petmacs-modeline-style
  'awesome-tray
  ;; 'doom-modeline
  "awesome-tray or doom-modeline. doom-modeline is default in TUI.")

(defvar petmacs-enable-mini-frame nil
  "enable mini frame feature")

(defvar petmacs-enable-display-line-numbers nil
  "enable display-line-numbers mode")

(defvar petmacs-lsp-mode-impl
  ;; 'lsp-mode
  'eglot-mode
  ;; 'lsp-bridge-mode

  "lsp-mode / eglot-mode / lsp-bridge-mode")


(defvar petmacs-dap-mode-impl
  'dap-mode
  ;; 'dape
  "dap-mode / dape ")

(defvar petmacs-lsp-active-modes '(
				                   c-mode
				                   c-ts-mode
			                       c++-mode
			                       c++-ts-mode
				                   python-mode
				                   python-ts-mode
				                   java-mode
				                   java-ts-mode
				                   scala-mode
				                   scala-ts-mode
				                   go-mode
				                   go-ts-mode
				                   sh-mode
				                   sh-ts-mode
				                   )
  "Primary major modes of the lsp activated layer.")

(defvar petmacs-lsp-format-on-save-ignore-modes
  '(c-mode c++-mode python-mode markdown-mode))

(defvar  petmacs-proxy "winhost:1080"
  "Set network proxy.")

(defvar petmacs-socks-proxy "127.0.0.1:1086"
  "Set SOCKS proxy.")

(defface petmacs-favor-color-face
  `((((background light)) :foreground ,petmacs-favor-color :bold t)
    (t :foreground ,petmacs-favor-color :bold t))
  "petmacs favorite color face"
  :group 'basic-faces)

(provide 'init-custom)
