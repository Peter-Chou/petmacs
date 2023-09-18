;; -*- lexical-binding: t no-byte-compile: t -*-

(require 'init-const)
(require 'init-funcs)

(use-package corfu
  :bind (:map corfu-map
         ("C-M-m" . corfu-move-to-minibuffer))
  ;; :hook (corfu-mode . corfu-indexed-mode)
  :init
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-prefix 2
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match 'separator
        corfu-preselect 'first
        corfu-preview-current nil
        ;; corfu-auto-delay 0.1
        ;; corfu-popupinfo-delay '(0.2 . 0.1)
        )
  (when (> (frame-pixel-width) 3000) (custom-set-faces '(corfu-default ((t (:height 1.3))))))
  (global-corfu-mode)

  (defun corfu-beginning-of-prompt ()
    "Move to beginning of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (car completion-in-region--data)))

  (defun corfu-end-of-prompt ()
    "Move to end of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (cadr completion-in-region--data)))

  (define-key corfu-map [remap move-beginning-of-line] #'corfu-beginning-of-prompt)
  (define-key corfu-map [remap move-end-of-line] #'corfu-end-of-prompt)

  (with-eval-after-load 'lsp-mode
    (setq lsp-completion-provider :none) ;; we use Corfu!

    (defun petmacs/lsp-mode-setup-completion ()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless))) ;; Configure orderless

    (add-hook 'lsp-completion-mode-hook #'petmacs/lsp-mode-setup-completion)))

;; M-x kind-icon-preview-all to reset and preview all icons after installation
(use-package kind-icon
  :quelpa (kind-icon :fetcher github
  		             :repo "jdtsmith/kind-icon"
  		             :files ("*.el"))
  :after corfu
  :init
  (require 'kind-icon)
  ;; to compute blended backgrounds correctly
  (when (icons-displayable-p)
    (setq ;; kind-icon-default-face 'corfu-default
     kind-icon-use-icons nil
     kind-icon-mapping
     `(
       (array ,(nerd-icons-codicon "nf-cod-symbol_array") :face font-lock-type-face)
       (boolean ,(nerd-icons-codicon "nf-cod-symbol_boolean") :face font-lock-builtin-face)
       (class ,(nerd-icons-codicon "nf-cod-symbol_class") :face font-lock-type-face)
       (color ,(nerd-icons-codicon "nf-cod-symbol_color") :face success)
       (command ,(nerd-icons-codicon "nf-cod-terminal") :face default)
       (constant ,(nerd-icons-codicon "nf-cod-symbol_constant") :face font-lock-constant-face)
       (constructor ,(nerd-icons-codicon "nf-cod-triangle_right") :face font-lock-function-name-face)
       (enummember ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
       (enum-member ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
       (enum ,(nerd-icons-codicon "nf-cod-symbol_enum") :face font-lock-builtin-face)
       (event ,(nerd-icons-codicon "nf-cod-symbol_event") :face font-lock-warning-face)
       (field ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-variable-name-face)
       (file ,(nerd-icons-codicon "nf-cod-symbol_file") :face font-lock-string-face)
       (folder ,(nerd-icons-codicon "nf-cod-folder") :face font-lock-doc-face)
       (interface ,(nerd-icons-codicon "nf-cod-symbol_interface") :face font-lock-type-face)
       (keyword ,(nerd-icons-codicon "nf-cod-symbol_keyword") :face font-lock-keyword-face)
       (macro ,(nerd-icons-codicon "nf-cod-symbol_misc") :face font-lock-keyword-face)
       (magic ,(nerd-icons-codicon "nf-cod-wand") :face font-lock-builtin-face)
       (method ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
       (function ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
       (module ,(nerd-icons-codicon "nf-cod-file_submodule") :face font-lock-preprocessor-face)
       (numeric ,(nerd-icons-codicon "nf-cod-symbol_numeric") :face font-lock-builtin-face)
       (operator ,(nerd-icons-codicon "nf-cod-symbol_operator") :face font-lock-comment-delimiter-face)
       (param ,(nerd-icons-codicon "nf-cod-symbol_parameter") :face default)
       (property ,(nerd-icons-codicon "nf-cod-symbol_property") :face font-lock-variable-name-face)
       (reference ,(nerd-icons-codicon "nf-cod-references") :face font-lock-variable-name-face)
       (snippet ,(nerd-icons-codicon "nf-cod-symbol_snippet") :face font-lock-string-face)
       (string ,(nerd-icons-codicon "nf-cod-symbol_string") :face font-lock-string-face)
       (struct ,(nerd-icons-codicon "nf-cod-symbol_structure") :face font-lock-variable-name-face)
       (text ,(nerd-icons-codicon "nf-cod-text_size") :face font-lock-doc-face)
       (typeparameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
       (type-parameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
       (unit ,(nerd-icons-codicon "nf-cod-symbol_ruler") :face font-lock-constant-face)
       (value ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-builtin-face)
       (variable ,(nerd-icons-codicon "nf-cod-symbol_variable") :face font-lock-variable-name-face)
       (t ,(nerd-icons-codicon "nf-cod-code") :face font-lock-warning-face)))
    )
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;; Use dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

(use-package cape
  :preface
  (defun petmacs/set-lsp-capfs ()
	(setq-local completion-at-point-functions
				(list (cape-super-capf
					   #'yasnippet-capf
					   #'lsp-completion-at-point)
					  #'cape-file
					  #'cape-dabbrev)))
  ;; :bind (("C-M-o" . cape-file))
  :hook (lsp-completion-mode . petmacs/set-lsp-capfs)
  :init (setq cape-dabbrev-min-length 2
              cape-dabbrev-check-other-buffers nil)
  :config
  ;; 默认用这三个补全后端
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(unless (display-graphic-p)
  (use-package popon
    :ensure nil)

  (use-package corfu-terminal
    :ensure nil
    :init
    (require 'corfu-terminal)
    (corfu-terminal-mode +1))
  )

(provide 'init-corfu)
