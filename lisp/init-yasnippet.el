;; -*- lexical-binding: t no-byte-compile: t -*-

(use-package yasnippet
  :diminish yas-minor-mode
  :bind ("M-e" . yas-expand)
  :hook (after-init . yas-global-mode)
  :config
  (setq hippie-expand-try-functions-list
        '(yas/hippie-try-expand
          try-complete-file-name-partially
          try-expand-all-abbrevs
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

(use-package yasnippet-snippets)

;; Yasnippet Completion At Point Function
(use-package yasnippet-capf
  :after cape
  :commands yasnippet-capf
  :functions cape-capf-super eglot-completion-at-point my-eglot-capf-with-yasnippet
  :init
  (add-to-list 'completion-at-point-functions #'yasnippet-capf)

  ;; To integrate `yasnippet-capf' with `eglot' completion
  ;; https://github.com/minad/corfu/wiki#making-a-cape-super-capf-for-eglot
  (defun my-eglot-capf-with-yasnippet ()
    (setq-local completion-at-point-functions
                (list
	             (cape-capf-super
		          #'eglot-completion-at-point
		          #'yasnippet-capf))))
  (add-hook 'eglot-managed-mode-hook #'my-eglot-capf-with-yasnippet))

(provide 'init-yasnippet)
