;; init-tools.el --- Setup useful tools.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package which-key
  :diminish
  :bind ("C-h M-m" . which-key-show-major-mode)
  :hook (after-init . which-key-mode)
  :init
  (setq which-key-idle-delay 0.2)
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix " ")

  ;; Needed to avoid nil variable error before update to recent which-key
  (defvar which-key-replacement-alist nil)
  ;; Reset to the default or customized value before adding our values in order
  ;; to make this initialization code idempotent.
  (custom-reevaluate-setting 'which-key-replacement-alist)
  ;; Replace rules for better naming of functions
  (let ((new-descriptions
         ;; being higher in this list means the replacement is applied later
         '(
           ("petmacs/\\(.+\\)" . "\\1")
           ("petmacs/toggle-\\(.+\\)" . "\\1")
           ("avy-goto-word-or-subword-1" . "avy word")
           ("shell-command" . "shell cmd")
           ("universal-argument" . "universal arg")
           ("er/expand-region" . "expand region")
	   ("counsel-projectile-rg". "project rg")
           ("evil-lisp-state-\\(.+\\)" . "\\1")
           ("helm-mini\\|ivy-switch-buffer" . "list-buffers"))))
    (dolist (nd new-descriptions)
      ;; ensure the target matches the whole string
      (push (cons (cons nil (concat "\\`" (car nd) "\\'")) (cons nil (cdr nd)))
            which-key-replacement-alist))))

(use-package hungry-delete
  :hook (after-init . global-hungry-delete-mode)
  :config
  (setq-default hungry-delete-chars-to-skip " \t\f\v"))

(use-package expand-region
  :init
  (setq expand-region-contract-fast-key "V"
        expand-region-reset-fast-key "r"))

(use-package editorconfig
  :diminish
  :hook (after-init . editorconfig-mode))

(use-package rg
  :hook (after-init . rg-enable-default-bindings)
  :config
  (setq rg-group-result t)
  (setq rg-show-columns t)

  (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)

  (with-eval-after-load 'projectile
    (defalias 'projectile-ripgrep 'rg-project)
    (bind-key "s R" #'rg-project projectile-command-map)))

(use-package avy
  :defer nil
  :commands (avy-with)
  :init
  (setq avy-timeout-seconds 0.0))

(use-package centered-cursor-mode)

(use-package general)
(use-package restart-emacs)
(use-package focus)                     ; Focus on the current region
(use-package carbon-now-sh)
(use-package daemons)                   ; system services/daemons
(use-package diffview)                  ; side-by-side diff view
(use-package esup)                      ; Emacs startup profiler


(provide 'init-tools)

;;; init-tools.el ends here
