;; init-company.el --- Setup company related packages.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package company
  :diminish company-mode
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-abort
  :bind (("M-/" . company-complete)
	 ("<backtab>" . company-yasnippet)
	 :map company-active-map
	 ("C-p" . company-select-previous)
	 ("C-n" . company-select-next)
	 ("<tab>" . company-complete-selection)
	 ;; ("C-/" . company-search-candidates)
	 ("C-/" . counsel-company)
	 ("C-M-/" . company-filter-candidates)
	 :map company-search-map
	 ("C-p" . company-select-previous)
	 ("C-n" . company-select-next)
	 ;; ("C-/" . company-search-candidates)
	 ("C-/" . counsel-company)
	 ("C-M-/" . company-filter-candidates)
	 ("C-d" . company-show-doc-buffer))
  :hook (after-init . global-company-mode)
  :config
  (setq company-tooltip-align-annotations t ; aligns annotation to the right
	company-tooltip-limit 12            ; bigger popup window
	company-idle-delay .1               ; decrease delay before autocompletion popup shows
	company-echo-delay 0                ; remove annoying blinking
	company-minimum-prefix-length 2
	company-require-match nil
	company-dabbrev-ignore-case nil
	company-dabbrev-downcase nil)
  (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete))

(when emacs/>=26p
  (use-package company-box
    :diminish
    :functions (all-the-icons-faicon
                all-the-icons-material
                all-the-icons-octicon
                all-the-icons-alltheicon)
    :hook (company-mode . company-box-mode)
    :init (setq company-box-enable-icon (display-graphic-p))
    :config
    (setq company-box-backends-colors nil)

    (with-eval-after-load 'all-the-icons
      (eval-and-compile
        (defun petmacs//company-box-icon (family icon &rest args)
          "Defines icons using `all-the-icons' for `company-box'."
          (when icon
            (let ((icon (pcase family
                          ('octicon (all-the-icons-octicon icon :v-adjust -0.05 args))
                          ('faicon (all-the-icons-faicon icon :v-adjust -0.0575))
                          ('material (all-the-icons-material icon :v-adjust -0.225 args))
                          ('alltheicon (all-the-icons-alltheicon icon args)))))
              (unless (symbolp icon)
                (concat icon
                        (propertize " " 'face 'variable-pitch)))))))

      (setq company-box-icons-unknown
            (petmacs//company-box-icon 'octicon "file-text"))

      (setq company-box-icons-elisp
            (list
             (petmacs//company-box-icon 'faicon "cube")        ; Function
             (petmacs//company-box-icon 'faicon "tag")         ; Variable
             (petmacs//company-box-icon 'faicon "cog")         ; Feature
             (petmacs//company-box-icon 'material "palette")   ; Face
             ))

      (setq company-box-icons-yasnippet
            (petmacs//company-box-icon 'octicon "file-code"))  ; Snippet

      (setq company-box-icons-lsp
            `(( 1  . ,(petmacs//company-box-icon 'faicon "file-text-o"))     ; Text
              ( 2  . ,(petmacs//company-box-icon 'faicon "cube"))            ; Method
              ( 3  . ,(petmacs//company-box-icon 'faicon "cube"))            ; Function
              ( 4  . ,(petmacs//company-box-icon 'faicon "cube"))            ; Constructor
              ( 5  . ,(petmacs//company-box-icon 'faicon "tag"))             ; Field
              ( 6  . ,(petmacs//company-box-icon 'faicon "tag"))             ; Variable
              ( 7  . ,(petmacs//company-box-icon 'faicon "cog"))             ; Class
              ( 8  . ,(petmacs//company-box-icon 'faicon "cogs"))            ; Interface
              ( 9  . ,(petmacs//company-box-icon 'alltheicon "less"))        ; Module
              (10  . ,(petmacs//company-box-icon 'faicon "wrench"))          ; Property
              (11  . ,(petmacs//company-box-icon 'faicon "tag"))             ; Unit
              (12  . ,(petmacs//company-box-icon 'faicon "tag"))             ; Value
              (13  . ,(petmacs//company-box-icon 'faicon "file-text-o"))     ; Enum
              (14  . ,(petmacs//company-box-icon 'material "format_align_center")) ; Keyword
              (15  . ,(petmacs//company-box-icon 'material "content_paste")) ; Snippet
              (16  . ,(petmacs//company-box-icon 'material "palette"))       ; Color
              (17  . ,(petmacs//company-box-icon 'faicon "file"))            ; File
              (18  . ,(petmacs//company-box-icon 'faicon "tag"))             ; Reference
              (19  . ,(petmacs//company-box-icon 'faicon "folder"))          ; Folder
              (20  . ,(petmacs//company-box-icon 'faicon "tag"))             ; EnumMember
              (21  . ,(petmacs//company-box-icon 'faicon "tag"))             ; Constant
              (22  . ,(petmacs//company-box-icon 'faicon "cog"))             ; Struct
              (23  . ,(petmacs//company-box-icon 'faicon "bolt"))            ; Event
              (24  . ,(petmacs//company-box-icon 'faicon "tag"))             ; Operator
              (25  . ,(petmacs//company-box-icon 'faicon "cog"))             ; TypeParameter
              )))))

(provide 'init-company)

;;; init-company.el ends here
