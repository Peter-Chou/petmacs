;; -*- lexical-binding: t no-byte-compile: t -*-

;; (use-package yasnippet
;;   :diminish yas-minor-mode
;;   :bind ("M-e" . yas-expand)
;;   :hook (after-init . yas-global-mode)
;;   :config
;;   (setq hippie-expand-try-functions-list
;;         '(yas/hippie-try-expand
;;           try-complete-file-name-partially
;;           try-expand-all-abbrevs
;;           try-expand-dabbrev
;;           try-expand-dabbrev-all-buffers
;;           try-expand-dabbrev-from-kill
;;           try-complete-lisp-symbol-partially
;;           try-complete-lisp-symbol)))

;; Yet another snippet extension
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

;; Collection of yasnippet snippets
(use-package yasnippet-snippets)

;; Yasnippet Completion At Point Function
(use-package yasnippet-capf
  :commands yasnippet-capf
  :functions cape-capf-super eglot-completion-at-point
  :hook (((conf-mode prog-mode text-mode) . my/yasnippet-capf-h)
         (eglot-managed-mode . my/eglot-capf))
  :init
  (defun my/yasnippet-capf-h ()
    (add-to-list 'completion-at-point-functions #'yasnippet-capf))

  ;; Making a Cape Super Capf for Eglot
  ;; https://github.com/minad/corfu/wiki#making-a-cape-super-capf-for-eglot
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list
	             (cape-capf-super
		          #'eglot-completion-at-point
		          #'yasnippet-capf)))))

(provide 'init-yasnippet)
