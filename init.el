;;; init.el --- Petmacs configurations  -*- lexical-binding: t no-byte-compile: t -*-

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
;; (require 'init-workspace)

(if (equal petmacs-checker 'flycheck)
    (require 'init-flycheck)
  (require 'init-flymake))

(require 'init-vcs)

(require 'init-projectile)
(require 'init-project)
(require 'init-dired)
(require 'init-shell)
(require 'init-yasnippet)

(require 'init-consult)

(when (petmacs-treesit-available-p)
  (require 'init-treesit))

(require 'init-corfu)

(cond ((equal petmacs-lsp-mode-impl 'eglot)
       (require 'init-eglot))
      (t
       (require 'init-lsp)))

(cond ((and (equal petmacs-dap-mode-impl 'dape) emacs/>=29p)
       (require 'init-dape))
      (t
       (require 'init-dap)))

(require 'init-elisp)
(require 'init-c-c++)
(require 'init-python)
(require 'init-java)
(require 'init-markdown)

;; (require 'init-golang)
;; (require 'init-scala)
;; (require 'init-web)

(require 'init-org)

(require 'init-keybindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
