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

(use-package yasnippet-capf
  :init (add-to-list 'completion-at-point-functions #'yasnippet-capf))


(provide 'init-yasnippet)
