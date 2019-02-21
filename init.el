
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

(require 'init-custom)
(require 'init-package)
(require 'init-foremost)
(require 'init-builtin)
(require 'init-font)

(require 'init-evil)

(require 'init-ui)
(require 'init-ivy)
(require 'init-company)
(require 'init-yasnippet)
(require 'init-dired)
(require 'init-treemacs)

(require 'init-tools)
(require 'init-highlight)
(require 'init-ibuffer)
(require 'init-vc)
(require 'init-project)

(require 'init-program)
(require 'init-lsp)
(require 'init-elisp)
(require 'init-c-c++)
(require 'init-python)

(require 'init-eshell)
(require 'init-misc)
(require 'init-default)

(require 'core-funcs)
(require 'leader-keybindings)
