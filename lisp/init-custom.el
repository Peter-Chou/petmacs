;; -*- lexical-binding: t no-byte-compile: t -*-

(eval-when-compile
  (require 'package))

(require 'init-const)

(defvar petmacs-checker 'flymake
  "flycheck / flymake ")

(defvar petmacs-lsp-mode-impl 'eglot
  "lsp-mode / eglot")

(defvar petmacs-dap-mode-impl 'dape
  "dap-mode / dape ")

(defvar petmacs-use-lsp-booster t
  "enable emacs lsp booster or not")

(defvar petmacs-icon t
  "Display icons or not.")

(defvar petmacs-treesit t
  "Enable tree-sitter or not. Native tree-sitter is introduced in 29.")

(defvar petmacs-completion-style 'minibuffer
  "minibuffer / childframe")

(defvar petmacs-favor-color "#F37022"
  "petmacs favor color")

(defface petmacs-favor-color-face
  `((((background light)) :foreground ,petmacs-favor-color :bold t)
    (t :foreground ,petmacs-favor-color :bold t))
  "petmacs favorite color face"
  :group 'basic-faces)

(defvar petmacs-font
  ;; "ComicCodeLigatures Nerd Font"
  ;; "MonegoLigatures Nerd Font"
  "JetBrains Mono NL"
  "default font")

(defvar petmacs-chinese-font "Maple Mono SC NF"
  "chinese font")

(defvar petmacs-font-size 14.0
  "font size")

(defvar petmacs-sidebar-width 30
  "sidebar width")

(defvar petmacs-ultra-screen-font-size 17.8
  "font size for ultra screen")

(defvar petmacs-ultra-sidebar-width 35
  "sidebar width in ultra screen")

(defvar petmacs-disable-modeline nil
  ;; (display-graphic-p)
  "disable modeline")

(defvar petmacs-enable-ligatures t
  "enable ligatures")

(defvar  petmacs-day-night-themes '(("8:00" . catppuccin)
                                    ("20:00"  . catppuccin))
  "day night theme")

(defvar petmacs-enable-mini-frame nil
  "enable mini frame feature")

(defvar petmacs-enable-display-line-numbers t
  "enable display-line-numbers mode")

(defvar  petmacs-proxy "winhost:1080"
  "Set network proxy.")

(defvar petmacs-socks-proxy "127.0.0.1:1080"
  "Set SOCKS proxy.")

(defvar petmacs-lsp-active-modes '(
                                   c-mode c-ts-mode c++-mode c++-ts-mode
                                   cmake-mode cmake-ts-mode
				                   python-mode python-ts-mode
				                   java-mode java-ts-mode
				                   scala-mode scala-ts-mode
				                   go-mode go-ts-mode
				                   sh-mode sh-ts-mode bash-ts-mode
                                   dockerfile-mode dockerfile-ts-mode)
  "Primary major modes of the lsp activated layer.")

(provide 'init-custom)
