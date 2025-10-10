;;; init.el --- Petmacs configurations  -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:
;;
;; Petmacs Emacs - Vim in Emacs
;;

;;; Code:

(when (version< emacs-version "29.1")
  (error "This requires Emacs 29.1 and above!"))

;;
;; Speed up Startup Process
;;

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

(unless (or (daemonp) noninteractive init-file-debug)
  ;; Temporarily suppress file-handler processing to speed up startup
  (let ((default-handlers file-name-handler-alist))
    (setq file-name-handler-alist nil)
    ;; Recover handlers after startup
    (add-hook 'emacs-startup-hook
              (lambda ()
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist default-handlers))))
              101)))

;;
;; Configure Load Path
;;

;; Add "lisp" and "site-lisp" to the beginning of `load-path`
(defun update-load-path (&rest _)
  "Update the `load-path` to prioritize personal configurations."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

;; Add subdirectories inside "site-lisp" to `load-path`
(defun add-subdirs-to-load-path (&rest _)
  "Recursively add subdirectories in `site-lisp` to `load-path`.

Avoid placing large files like EAF in `site-lisp` to prevent slow startup."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

;; Ensure these functions are called after `package-initialize`
(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

;; Initialize load paths explicitly
(update-load-path)

(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

;; Packages
(require 'init-package)

;; Preferences
(require 'init-basic)
(require 'init-font)

(require 'init-evil)
(require 'init-tools)

(require 'init-ui)
(require 'init-highlight)
(require 'init-dashboard)
(require 'init-ibuffer)
(require 'init-window)
(require 'init-dired)
(require 'init-treemacs)
;; (require 'init-workspace)

;; (require 'init-project)
(require 'init-projectile)
(require 'init-vcs)
(require 'init-yasnippet)
(require 'init-shell)
(require 'init-flymake)

;; treesit
(when (petmacs-treesit-available-p)
  (require 'init-treesit))

;; completion
(require 'init-consult)
(require 'init-corfu)

;; lsp & dap
(require 'init-eglot)
(require 'init-dape)

;; org
(require 'init-org)

;; Programming
(require 'init-elisp)
(require 'init-c-c++)
(require 'init-python)
(require 'init-java)
(require 'init-markdown)
(require 'init-web)

;; keybindings
(require 'init-keybindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
