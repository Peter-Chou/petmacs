;;; init.el --- Petmacs configurations  -*- lexical-binding: t no-byte-compile: t -*-

;; Speed up startup
(setq auto-mode-case-fold nil)

(unless (or (daemonp) noninteractive init-file-debug)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist
                                           old-file-name-handler-alist)))))))


;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(update-load-path)

(require 'init-custom)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (if (equal petmacs-lsp-client-mode 'lsp-mode)
                (setq gc-cons-threshold 100000000)
              (setq gc-cons-threshold 800000))))

(require 'init-funcs)

(require 'init-package)
(require 'init-basic)
(require 'init-font)

(require 'init-evil)

(require 'init-ui)
(require 'init-highlight)
(require 'init-dashboard)
(require 'init-ibuffer)
(require 'init-treemacs)

(require 'init-flycheck)
(require 'init-edit)
(require 'init-tools)
(require 'init-vcs)

(require 'init-projectile)
(require 'init-dired)
(require 'init-shell)
(require 'init-snippets)

(require 'init-consult)

(if (and (equal petmacs-lsp-client-mode 'lsp-bridge-mode) (display-graphic-p))
    (require 'init-lsp-bridge)
  (progn
    (require 'init-corfu)
    (require 'init-lsp)))

(require 'init-dap)

(require 'init-elisp)
(require 'init-c-c++)
(require 'init-python)
(require 'init-golang)
(require 'init-java)
(require 'init-scala)

(require 'init-markdown)
(require 'init-yaml)

(require 'init-org)

(if (and (display-graphic-p)
         (file-directory-p (petmacs/get-eaf-app-directory)))
    (require 'init-eaf))

(require 'core-funcs)
(require 'init-keybindings)
