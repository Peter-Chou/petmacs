;;; init.el --- Petmacs configurations  -*- lexical-binding: t no-byte-compile: t -*-

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

(unless (or (daemonp) noninteractive init-file-debug)
  ;; Suppress file handlers operations at startup
  ;; `file-name-handler-alist' is consulted on each call to `require' and `load'
  (let ((old-value file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist old-value))))
              101)))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'.
  Don't put large files in `site-lisp' directory,
   Otherwise the startup will be very slow."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

(require 'init-custom)

(require 'init-funcs)

(require 'init-package)
(require 'init-basic)
(require 'init-font)

(require 'init-evil)
(require 'init-tools)

(require 'init-ui)
(require 'init-highlight)
(require 'init-dashboard)
(require 'init-ibuffer)
(require 'init-window)
(require 'init-treemacs)

(require 'init-flymake)
;; (require 'init-flycheck)
(require 'init-vcs)

(require 'init-projectile)
(require 'init-project)
;; (require 'init-persp)
(require 'init-dired)
(require 'init-shell)
(require 'init-yasnippet)

(require 'init-consult)

(when (petmacs-treesit-available-p)
  (require 'init-treesit))

(cond ((equal petmacs-lsp-mode-impl 'lsp-bridge-mode)
       (require 'init-lsp-bridge))
      (t
       (require 'init-corfu)
       (require 'init-lsp)))
(require 'init-eglot)

(cond ((and (equal petmacs-dap-mode-impl 'dape) emacs/>=29p)
       (require 'init-dape))
      (t
       (require 'init-dap)))

(require 'init-elisp)
(require 'init-c-c++)
(require 'init-python)
(require 'init-golang)
(require 'init-java)
(require 'init-scala)

(require 'init-web)
(require 'init-markdown)
(require 'init-markup)

(require 'init-org)

(require 'core-funcs)
(require 'init-keybindings)

(toggle-frame-fullscreen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
