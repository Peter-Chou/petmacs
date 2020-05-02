;; init-company.el --- Setup company related packages.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(use-package company
  :diminish
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-abort
  :bind (("M-/" . company-complete)
	 ("<backtab>" . company-yasnippet)
	 :map company-active-map
	 ("C-p" . company-select-previous)
	 ("C-n" . company-select-next)
	 ("<tab>" . company-complete-selection)
	 ("C-/" . counsel-company)
	 ;; ("C-M-/" . company-filter-candidates)
	 ("C-d" . company-show-doc-buffer)
	 :map company-search-map
	 ("C-p" . company-select-previous)
	 ("C-n" . company-select-next)
	 ;; ("C-/" . company-search-candidates)
	 ;; ("C-M-/" . company-filter-candidates)
	 ("C-d" . company-show-doc-buffer))
  :hook (after-init . global-company-mode)
  :config
  (setq company-tooltip-align-annotations t ; aligns annotation to the right
	company-tooltip-limit 12            ; bigger popup window
	company-idle-delay 0.0               ; decrease delay before autocompletion popup shows
	company-echo-delay (if (display-graphic-p) nil 0)
	company-minimum-prefix-length 1
	company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-global-modes '(not erc-mode message-mode help-mode gud-mode eshell-mode shell-mode)
	company-backends '((company-files          ; files & directory
			    company-keywords       ; keywords
			    company-capf
			    company-yasnippet)
			   (company-abbrev company-dabbrev))
	company-frontends '(company-pseudo-tooltip-frontend
			    company-echo-metadata-frontend))
  (define-key global-map (kbd "C-M-i") 'company-complete)
  (define-key global-map (kbd "C-M-k") 'company-files)

  ;; Better sorting and filtering
  (use-package company-prescient
    :init (company-prescient-mode 1))

  ;; Icons and quickhelp
  (when emacs/>=26p
    (use-package company-box
      :diminish
      :defines company-box-icons-all-the-icons
      :hook (company-mode . company-box-mode)
      :init (setq company-box-enable-icon t
                  company-box-backends-colors nil
                  company-box-show-single-candidate t
                  company-box-max-candidates 50
                  company-box-doc-delay 0.2)
      :config
      (with-no-warnings
        ;; Prettify icons
        (defun my-company-box-icons--elisp (candidate)
          (when (derived-mode-p 'emacs-lisp-mode)
            (let ((sym (intern candidate)))
              (cond ((fboundp sym) 'Function)
                    ((featurep sym) 'Module)
                    ((facep sym) 'Color)
                    ((boundp sym) 'Variable)
                    ((symbolp sym) 'Text)
                    (t . nil)))))
        (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp))

      (when (icons-displayable-p)
        (declare-function all-the-icons-faicon 'all-the-icons)
        (declare-function all-the-icons-material 'all-the-icons)
        (declare-function all-the-icons-octicon 'all-the-icons)
        (setq company-box-icons-all-the-icons
              `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))
                (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
                (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
                (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
                (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
                (Field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
                (Variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
                (Class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
                (Interface . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
                (Module . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
                (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
                (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.8 :v-adjust -0.15))
                (Value . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
                (Enum . ,(all-the-icons-material "storage" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
                (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
                (Snippet . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
                (Color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.15))
                (File . ,(all-the-icons-faicon "file-o" :height 0.8 :v-adjust -0.02))
                (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
                (Folder . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
                (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
                (Constant . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
                (Struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
                (Event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
                (Operator . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
                (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
                (Template . ,(all-the-icons-material "format_align_left" :height 0.8 :v-adjust -0.15)))
              company-box-icons-alist 'company-box-icons-all-the-icons))))

  ;; Popup documentation for completion candidates
  (when (and (not emacs/>=26p) (display-graphic-p))
    (use-package company-quickhelp
      :defines company-quickhelp-delay
      :bind (:map company-active-map
             ([remap company-show-doc-buffer] . company-quickhelp-manual-begin))
      :hook (global-company-mode . company-quickhelp-mode)
      :init (setq company-quickhelp-delay 0.5))))

(provide 'init-company)

;;; init-company.el ends here
