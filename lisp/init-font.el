;; init-font.el --- Setup fonts.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(set-face-attribute 'default nil :font (format "JetBrains Mono-%S" petmacs-font-size))
(setq-default line-spacing 0.2) ; add 0.2 height between lines

;; fix the delay when showing text in chinese
(if window-system
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
			;; (font-spec :family "Microsoft Yahei" :size 18.5))
			(font-spec :family "等距更纱黑体 SC" :size petmacs-font-size))))


;; Specify font for all unicode characters
(cl-loop for font in '("Apple Color Emoji" "Symbola" "Symbol")
         when (font-installed-p font)
         return(set-fontset-font t 'unicode font nil 'prepend))

;; Specify font for Chinese characters
(cl-loop for font in '("WenQuanYi Micro Hei" "Microsoft Yahei")
         when (font-installed-p font)
         return (set-fontset-font t '(#x4e00 . #x9fff) font))


(use-package fontify-face)

(provide 'init-font)

;;; init-font.el ends here
