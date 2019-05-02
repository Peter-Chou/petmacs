;;; init.el --- Petmacs configurations  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(defvar file-name-handler-alist-old file-name-handler-alist)

(require 'package)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

(add-hook 'after-init-hook
	  `(lambda ()
	     (setq file-name-handler-alist file-name-handler-alist-old
		   gc-cons-threshold 800000
		   gc-cons-percentage 0.1)
	     (garbage-collect)) t)

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

;; Initialize packages
(package-initialize)

(require 'init-custom)
(require 'init-const)
(require 'init-variable)
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
(require 'init-treemacs)  ;TODO: need refactor

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
(require 'init-org)
(require 'init-markdown)
(require 'init-json)
(require 'init-yaml)
(require 'init-sql)

(require 'init-eshell)
(require 'init-misc)

(require 'core-funcs)
(require 'leader-keybindings)

;;; init.el ends here
