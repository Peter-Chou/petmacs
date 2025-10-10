;; -*- lexical-binding: t no-byte-compile: t -*-

(eval-when-compile
  (require 'init-const)
  (require 'init-funcs))

(use-package corfu
  :autoload (corfu-quit consult-completion-in-region)
  :functions (corfu-move-to-minibuffer)
  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  :bind (:map corfu-map
         ("C-M-m" . corfu-move-to-minibuffer)
         ("M-P" . corfu-popupinfo-scroll-down) ;; corfu-next
         ("M-N" . corfu-popupinfo-scroll-up) ;; corfu-previous
         )
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode))
  :init
  (setq corfu-auto t
        corfu-cycle t
        corfu-auto-prefix 2
        corfu-auto-delay 0.2
        corfu-min-width 80
        corfu-max-width 100
        corfu-popupinfo-delay '(0.4 . 0.2)
        corfu-popupinfo-max-width 120
        corfu-popupinfo-max-height 40
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match 'separator
        corfu-preselect 'first
        corfu-preview-current nil
        corfu-on-exact-match nil)
  (require 'corfu)
  (when (> (frame-pixel-width) 3000) (custom-set-faces '(corfu-default ((t (:height 1.3))))))

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
  :config
  (with-eval-after-load 'eglot
    (defun petmacs/eglot-capf-setup ()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless)) ;; Configure orderless
      (setq-local completion-at-point-functions (list
                                                 #'eglot-completion-at-point
                                                 #'cape-file
                                                 #'yasnippet-capf
    		                                     #'cape-dabbrev)))
    (add-hook 'eglot-managed-mode-hook #'petmacs/eglot-capf-setup))
  ;;Quit completion before saving
  (add-hook 'before-save-hook #'corfu-quit))

(use-package nerd-icons-corfu
  :autoload nerd-icons-corfu-formatter
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Use dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :init
  (setq dabbrev-upcase-means-case-search t
        dabbrev-check-all-buffers nil
        dabbrev-check-other-buffers t
        dabbrev-friend-buffer-function 'dabbrev--same-major-mode-p
        dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package cape
  :commands (cape-file cape-elisp-block cape-keyword)
  :autoload (cape-wrap-noninterruptible cape-wrap-nonexclusive cape-wrap-buster)
  :autoload (cape-wrap-silent cape-wrap-purify)
  :init (setq cape-dabbrev-check-other-buffers 'some
              ;; cape-dabbrev-check-other-buffers nil
              cape-dabbrev-min-length 2)
  :config
  ;; 默认用这三个补全后端
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)

  (advice-add 'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive)

  ;; Sanitize the `pcomplete-completions-at-point' Capf.  The Capf has undesired
  ;; side effects on Emacs 28.  These advices are not needed on Emacs 29 and newer.
  (unless emacs/>=29p
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)))

(unless (display-graphic-p)
  (use-package popon
    :ensure nil)

  (use-package corfu-terminal
    :ensure nil
    :init
    (require 'corfu-terminal)
    (corfu-terminal-mode +1)))

(provide 'init-corfu)
