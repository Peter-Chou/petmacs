;; -*- lexical-binding: t no-byte-compile: t -*-

(eval-when-compile
  (require 'package))

(require 'init-const)

(defvar petmacs-quelpa-use-gitee-mirror nil
  "use gitee mirror instead of github")

(defvar petmacs-checker
  ;; 'flycheck
  'flymake
  "flycheck / flymake ")

(defvar petmacs-lsp-mode-impl
  ;; 'lsp-mode
  'eglot
  "lsp-mode / eglot")

(defvar petmacs-dap-mode-impl
  ;; 'dap-mode
  'dape
  "dap-mode / dape ")

(defvar petmacs-icon t
  "Display icons or not.")

(defvar petmacs-treesit t
  "Enable tree-sitter or not. Native tree-sitter is introduced in 29.")

(defvar petmacs-completion-style
  'minibuffer
  ;; 'childframe
  "minibuffer / childframe")

(defvar petmacs-favor-color "#F37022"
  "petmacs favor color")

(defvar petmacs-font
  "JetBrains Mono"
  ;; "Monego Ligatures"
  "font")

(defvar  petmacs-chinese-font
  "Maple Mono SC NF"
  ;; "等距更纱黑体 SC"
  "chinese font")

(defvar  petmacs-font-size 16.5
  "font size")

(defvar  petmacs-ultra-screen-font-size 20.0
  "font size for ultra screen")

(defvar petmacs-disable-modeline nil
  ;; (display-graphic-p)
  "disable modeline")

(defvar petmacs-enable-ligatures t
  "enable ligatures")

(defvar petmacs-quelpa-checkout-melpa nil
  "update melpa, disable it if the network is unstable.")

(defvar  petmacs-day-night-themes
  ;; '(("8:00" . spacemacs-light)
  ;;   ("20:00"  . doom-dracula))
  ;; '(("8:00" . doom-shades-of-purple)
  ;;   ("20:00"  . doom-shades-of-purple))
  '(("8:00" . dracula)
    ("20:00"  . dracula))
  "day night theme")

(defvar petmacs-modeline-style
  'awesome-tray
  ;; 'doom-modeline
  "awesome-tray or doom-modeline. doom-modeline is default in TUI.")

(defvar petmacs-enable-mini-frame nil
  "enable mini frame feature")

(defvar petmacs-enable-display-line-numbers nil
  "enable display-line-numbers mode")

(defvar  petmacs-proxy "winhost:1080"
  "Set network proxy.")

(defvar petmacs-socks-proxy "127.0.0.1:1080"
  "Set SOCKS proxy.")

(defface petmacs-favor-color-face
  `((((background light)) :foreground ,petmacs-favor-color :bold t)
    (t :foreground ,petmacs-favor-color :bold t))
  "petmacs favorite color face"
  :group 'basic-faces)

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

(provide 'init-custom)
