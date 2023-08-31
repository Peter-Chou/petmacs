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

(push (expand-file-name "lisp" user-emacs-directory) load-path)

(require 'init-custom)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

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

;; (require 'init-flymake)
(require 'init-flycheck)
(require 'init-vcs)

(require 'init-projectile)
(require 'init-persp)
(require 'init-dired)
(require 'init-shell)
(require 'init-yasnippet)

(require 'init-consult)

(when (petmacs-treesit-available-p)
  (require 'init-tree-sitter))

(cond ((equal petmacs-lsp-client 'lsp-bridge-mode)
       (require 'init-lsp-bridge))
      (t
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
(require 'init-markup)

(require 'init-org)

(if (and (display-graphic-p)
         (file-directory-p (petmacs/get-eaf-app-directory)))
    (require 'init-eaf))

(require 'init-web)

(require 'core-funcs)
(require 'init-keybindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
