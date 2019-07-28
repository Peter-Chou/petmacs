;;; init.el --- Petmacs configurations  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:


(require 'package)

;; Speed up startup
(defvar petmacs-gc-cons-threshold (if (display-graphic-p) 8000000 800000)
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.")

(defvar petmacs-gc-cons-upper-limit (if (display-graphic-p) 400000000 100000000)
  "The temporary value for `gc-cons-threshold' to defer it.")

(defvar default-file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)
(setq gc-cons-threshold petmacs-gc-cons-upper-limit)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after startup."
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold petmacs-gc-cons-threshold)

            ;; GC automatically while unfocusing the frame
            ;; `focus-out-hook' is obsolete since 27.1
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
			      (lambda ()
				(unless (frame-focus-state)
				  (garbage-collect))))
              (add-hook 'focus-out-hook 'garbage-collect))

            ;; Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
            ;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
            (defun my-minibuffer-setup-hook ()
              (setq gc-cons-threshold petmacs-gc-cons-upper-limit))

            (defun my-minibuffer-exit-hook ()
              (setq gc-cons-threshold petmacs-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "site-lisp" user-emacs-directory) load-path)
  (push (expand-file-name "lisp" user-emacs-directory) load-path))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory
          (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)


(update-load-path)
(add-subdirs-to-load-path)

;; use mirror
(setq package-archives '(("gnu"          . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			 ("melpa"        . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
			 ("org"          . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
			 ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")))

(require 'init-custom)
(require 'init-const)
(require 'init-package)
(require 'init-default)
(require 'init-font)

(require 'init-evil)

(require 'init-ui)
(require 'init-window)
(require 'init-layout)
(require 'init-dashboard)

(require 'init-ivy)
(require 'init-company)
(require 'init-yasnippet)
(require 'init-dired)
(require 'init-treemacs)

(require 'init-tools)
(require 'init-highlight)
(require 'init-ibuffer)
(require 'init-vcs)
(require 'init-project)

(require 'init-program)
(require 'init-flycheck)
(require 'init-lsp)
(require 'init-elisp)
(require 'init-c-c++)
(require 'init-python)
(require 'init-java)
(require 'init-web)
(require 'init-org)
(require 'init-markdown)
(require 'init-yaml)
(require 'init-sql)

(require 'init-eshell)
(require 'init-misc)

(require 'core-funcs)
(require 'leader-keybindings)

;;; init.el ends here
