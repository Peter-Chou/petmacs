;;; init.el --- Petmacs configurations  -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:

;;; Code:


(require 'package)

;; Speed up startup
(defvar petmacs-gc-cons-threshold (if (display-graphic-p) 64000000 1600000)
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.")

(defvar petmacs-gc-cons-upper-limit (if (display-graphic-p) 512000000 128000000)
  "The temporary value for `gc-cons-threshold' to defer it.")

(defvar petmacs-gc-timer (run-with-idle-timer 10 t #'garbage-collect)
  "Run garbarge collection when idle 10s.")

(defvar default-file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)
(setq gc-cons-threshold petmacs-gc-cons-upper-limit
      gc-cons-percentage 0.5)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after startup."
            (setq file-name-handler-alist default-file-name-handler-alist
                  gc-cons-threshold petmacs-gc-cons-threshold
                  gc-cons-percentage 0.1)

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
            (add-hook 'minibuffer-setup-hook
                      (lambda ()
                        "Enlarge gc cons threshold while entering minibuffer."
                        (setq gc-cons-threshold petmacs-gc-cons-upper-limit)))
            (add-hook 'minibuffer-exit-hook
                      (lambda ()
                        "Recover gc cons threshold while exiting minibuffer."
                        (setq gc-cons-threshold petmacs-gc-cons-threshold)))))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)
(add-subdirs-to-load-path)

;; use mirror
(setq package-archives '(("gnu"          . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			 ;; ("melpa"        . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
			 ("melpa"        . "https://melpa.org/packages/")
			 ("org"          . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
			 ;; ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ))

(setq byte-compile-warnings '(cl-functions))

(require 'init-custom)
(require 'init-const)
(require 'init-package)
(require 'init-funcs)
(require 'init-default)
(require 'init-font)

(require 'init-evil)

(require 'init-ui)

(require 'init-window)
;; (require 'init-layout)
(require 'init-bookmark)
(require 'init-dashboard)

(require 'init-yasnippet)
(require 'init-ivy)
(require 'init-company)
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
(require 'init-dap)
(require 'init-elisp)
(require 'init-c-c++)
(require 'init-python)
(require 'init-java)
(require 'init-scala)
(require 'init-golang)
(require 'init-web)
(require 'init-org)
(require 'init-markdown)
(require 'init-yaml)
(require 'init-sql)

(require 'init-shell)
(require 'init-misc)

(require 'core-funcs)
(require 'init-leader)

;;; init.el ends here
