;; -*- lexical-binding: t no-byte-compile: t -*-

(require 'init-const)
(require 'init-funcs)

(use-package corfu
  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  :bind (:map corfu-map
         ("C-M-m" . corfu-move-to-minibuffer)
         ("M-P" . corfu-popupinfo-scroll-down) ;; corfu-next
         ("M-N" . corfu-popupinfo-scroll-up) ;; corfu-previous
         )
  :init
  (setq corfu-auto t
        corfu-cycle t
        corfu-auto-prefix 1
        corfu-auto-delay 0.2
        corfu-min-width 80
        corfu-max-width 100
        corfu-popupinfo-delay 0.6
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

  (pcase petmacs-lsp-mode-impl
    ('lsp-mode
     (with-eval-after-load 'lsp-mode
       (setq lsp-completion-provider :none) ;; we use Corfu!

       (defun petmacs/lsp-mode-setup-completion ()
         (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
               '(orderless))) ;; Configure orderless
       (add-hook 'lsp-completion-mode-hook #'petmacs/lsp-mode-setup-completion)
       (defun petmacs/set-lsp-capfs ()
         (setq-local completion-at-point-functions
      			     (list #'lsp-completion-at-point
                           #'cape-file
      			           #'cape-dabbrev)))
       (add-hook 'lsp-completion-mode-hook #'petmacs/set-lsp-capfs)))
    ('eglot-mode
     (defun petmacs/eglot-capf-setup ()

       (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
             '(orderless)) ;; Configure orderless

       (setq-local completion-at-point-functions
    		       (list
                    #'eglot-completion-at-point
                    #'cape-file
                    #'yasnippet-capf
    		        #'cape-dabbrev
                    )))
     (add-hook 'eglot-managed-mode-hook #'petmacs/eglot-capf-setup)))

  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Use dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

(use-package cape
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
    (corfu-terminal-mode +1)))

(provide 'init-corfu)
