;; -*- lexical-binding: t no-byte-compile: t -*-

(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

;; set default font
(if (font-installed-p petmacs-font)
    (set-face-attribute 'default nil :font (format "%s-%d" petmacs-font petmacs-font-size))
  (message  (format "%s font is not installed, please install it for better ui display." petmacs-font)))
(setq-default line-spacing 0.2) ; add 0.2 height between lines

(if (display-graphic-p)
    ;; set chinese font
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
			            (font-spec :family petmacs-chinese-font :size petmacs-font-size))))


;; Specify font for all unicode characters
(cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
         when (font-installed-p font)
         return (set-fontset-font t 'unicode font nil 'prepend))

;; Emoji
(cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji")
         when (font-installed-p font)
         return (if (>= emacs-major-version 28)
                    (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend)
                  (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))

;; Fontify symbols representing faces with that face
(use-package fontify-face)

(provide 'init-font)
