
(defvar petmacs--font-size 17.5
  "default font size")

;; (set-default-font "Fira Code Retina 14.5" nil t)
(set-face-attribute 'default nil :font (format "Fira Code Retina-%S" petmacs--font-size))

;; fix the delay when showing text in chinese
(dolist (charset '(kana han cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
                    ;; (font-spec :family "Microsoft Yahei" :size 18.5))
		    (font-spec :family "等距更纱黑体 SC" :size petmacs--font-size))
  )

(use-package fontify-face)

(provide 'init-font)
