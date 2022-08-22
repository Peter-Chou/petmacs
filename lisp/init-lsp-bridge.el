;; -*- lexical-binding: t no-byte-compile: t -*-

(require 'init-custom)

(use-package dumb-jump
  :init
  (setq dumb-jump-prefer-searcher 'rg)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;;; pip install epc
(use-package lsp-bridge
  :quelpa (lsp-bridge :fetcher github
    	              :repo "manateelazycat/lsp-bridge"
    	              :files ("*"))
  :preface
  ;; 融合 `lsp-bridge' `find-function' 以及 `dumb-jump' 的智能跳转
  (defun petmacs/lsp-bridge-jump ()
    (interactive)
    (cond
     ((eq major-mode 'emacs-lisp-mode)
      (evil-goto-definition))
     ((eq major-mode 'org-mode)
      (org-agenda-open-link))
     (lsp-bridge-mode
      (lsp-bridge-find-def))
     (t
      (require 'dumb-jump)
      (dumb-jump-go))))

  (defun petmacs/lsp-bridge-jump-back ()
    (interactive)
    (cond
     ((member major-mode petmacs-lsp-active-modes)
      (lsp-bridge-return-from-def))
     (t
      (require 'dumb-jump)
      (dumb-jump-back))))

  :init
  (require 'lsp-bridge)
  (setq lsp-bridge-python-command (expand-file-name "lsp-bridge/bin/python" (getenv "WORKON_HOME"))
        lsp-bridge-enable-candidate-doc-preview nil
        lsp-bridge-enable-signature-help t
        acm-enable-doc nil
        acm-candidate-match-function 'orderless-flex
        )
  :config
  ;; don't show lsp-bridge-info in modeline
  (setq mode-line-misc-info (delete '(lsp-bridge-mode (" [" lsp-bridge--mode-line-format "] "))
                                    mode-line-misc-info))

  (with-eval-after-load 'pyvenv
    (add-hook 'pyvenv-post-activate-hooks #'lsp-bridge-restart-process))

  (define-key evil-motion-state-map (kbd "C-o") #'petmacs/lsp-bridge-jump-back)
  (define-key evil-motion-state-map "gR" #'lsp-bridge-rename)
  (define-key evil-motion-state-map "gr" #'lsp-bridge-find-references)
  (define-key evil-normal-state-map "gi" #'lsp-bridge-find-impl)
  (define-key evil-motion-state-map "gd" #'petmacs/lsp-bridge-jump)
  (define-key evil-motion-state-map "gs" #'lsp-bridge-restart-process)
  (define-key evil-normal-state-map "gh" #'lsp-bridge-lookup-documentation)

  (evil-add-command-properties #'petmacs/lsp-bridge-jump :jump t)

  (evil-define-key 'normal lsp-bridge-ref-mode-map
    (kbd "RET") 'lsp-bridge-ref-open-file-and-stay
    "q" 'lsp-bridge-ref-quit)

  (global-lsp-bridge-mode))

(provide 'init-lsp-bridge)
