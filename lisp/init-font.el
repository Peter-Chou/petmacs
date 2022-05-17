;; init-font.el --- Setup fonts.  -*- lexical-binding: t -*-

(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

(set-face-attribute 'default nil :font (format "JetBrains Mono-%S" petmacs-font-size))
(setq-default line-spacing 0.2) ; add 0.2 height between lines

;; fix the delay when showing text in chinese
(if window-system
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
			;; (font-spec :family "Microsoft Yahei" :size 18.5))
			(font-spec :family "等距更纱黑体 SC" :size petmacs-font-size))))


;; Specify font for all unicode characters
(cl-loop for font in '("Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Symbol")
         when (font-installed-p font)
         return(set-fontset-font t 'unicode font nil 'prepend))

;; Fontify symbols representing faces with that face
(use-package fontify-face)

(provide 'init-font)

;;; init-font.el ends here

