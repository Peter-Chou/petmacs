;;	-*- lexical-binding: t -*-

(use-package corfu-doc)

;;; pip install epc
(use-package lsp-bridge
  :load-path "site-lisp"
  :ensure nil
  :init
  (require 'lsp-bridge)
  (require 'lsp-bridge-icon)
  (require 'lsp-bridge-orderless)
  (require 'lsp-bridge-jdtls)
  :config
  ;; 通过Cape融合不同的补全后端，比如lsp-bridge、 tabnine、 file、 dabbrev.
  (defun lsp-bridge-mix-multi-backends ()
    (setq-local completion-category-defaults nil)
    (setq-local completion-at-point-functions
                (list
                 (cape-capf-buster
                  (cape-super-capf
                   #'lsp-bridge-capf
                   #'cape-file
                   #'cape-dabbrev)
                  'equal))))

  (dolist (hook lsp-bridge-default-mode-hooks)
    (add-hook hook (lambda ()
                     (lsp-bridge-mix-multi-backends) ; 通过Cape融合多个补全后端
                     )))
  (global-lsp-bridge-mode))


(provide 'init-lsp-bridge)
