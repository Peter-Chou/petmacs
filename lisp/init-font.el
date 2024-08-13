;; -*- lexical-binding: t no-byte-compile: t -*-

(eval-when-compile
  (require 'init-const)
  (require 'init-custom)
  (require 'init-funcs))

(setq-default line-spacing 0.2) ; add 0.2 height between lines

;; set default font
(if (font-installed-p petmacs-font)
    (if sys/ultrap
        (set-face-attribute 'default nil :font (font-spec :name petmacs-font :size petmacs-ultra-screen-font-size))
      (set-face-attribute 'default nil :font (font-spec :name petmacs-font :size petmacs-font-size)))
  (message  (format "%s font is not installed, please install it for better ui display." petmacs-font)))

;; set chinese font
(if (display-graphic-p)
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
			            (font-spec :family petmacs-chinese-font))))

;; Specify font for all unicode characters
(cl-loop for font in '("Symbola" "Symbol" "Segoe UI Symbol")
         when (font-installed-p font)
         return (set-fontset-font t 'unicode font nil 'prepend))

;; Emoji
(cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
         when (font-installed-p font)
         return (cond
                 ((< emacs-major-version 27)
                  (set-fontset-font
                   "fontset-default" 'unicode font nil 'prepend))
                 ((< emacs-major-version 28)
                  (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))
                 (t
                  (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))))

(provide 'init-font)
