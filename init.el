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

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 800000)))

(setq byte-compile-warnings '(cl-functions))

(if (and (fboundp 'native-comp-available-p)
	 (native-comp-available-p))
    (progn
      (message "Native compilation is available")
      ;; native-compile all Elisp files under a directory
      (native-compile-async (expand-file-name "site-lisp" user-emacs-directory) 'recursively)
      (setq package-native-compile t
	    native-comp-async-report-warnings-errors nil))
  (message "Native complation is *not* available"))

(if (functionp 'json-serialize)
    (message "Native JSON is available")
  (message "Native JSON is *not* available"))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(update-load-path)

;; use mirror
(setq package-archives '(("gnu"          . "https://elpa.gnu.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ;; ("org"          . "https://orgmode.org/elpa/")

                         ;; ("elpa"         . "http://elpa.gnu.org/packages/")
			             ;; ("melpa"        . "http://melpa.org/packages/")
			             ))

(require 'init-package)
(require 'init-basic)
(require 'init-font)

(require 'init-evil)

(require 'init-git)
(require 'init-flycheck)
(require 'init-edit)
(require 'init-tools)

(require 'init-ui)
(require 'init-highlight)
(require 'init-dashboard)
(require 'init-ibuffer)
(require 'init-treemacs)

(require 'init-projectile)
(require 'init-dired)
(require 'init-shell)

(require 'init-consult)

(if (display-graphic-p)
    (require 'init-corfu)
  (require 'init-company))

(require 'init-lsp)
;; (require 'init-lsp-bridge)

(require 'init-elisp)
(require 'init-c-c++)
(require 'init-python)
(require 'init-golang)
(require 'init-java)
(require 'init-scala)

(require 'init-markdown)
(require 'init-yaml)

(require 'init-org)
(require 'init-snippets)
(require 'init-eaf)

(require 'core-funcs)
(require 'init-keybindings)
