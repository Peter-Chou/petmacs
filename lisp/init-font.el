;; init-font.el --- Setup fonts.  -*- lexical-binding: t -*-

(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

(if (font-installed-p petmacs-font)
    (set-face-attribute 'default nil :font (format "%s-%d" petmacs-font petmacs-font-size))
  (message  (format "%s font is not installed, please install it for better ui display." petmacs-font)))
(setq-default line-spacing 0.2) ; add 0.2 height between lines

;; fix the delay when showing text in chinese
(if window-system
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
			            ;; (font-spec :family "Microsoft Yahei" :size 18.5))
			            (font-spec :family "等距更纱黑体 SC" :size petmacs-font-size))))


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

;;; init-font.el ends here
