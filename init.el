;;; init.el --- Petmacs configurations  -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:

;;; Code:

;; Speed up startup
(setq auto-mode-case-fold nil)

(require 'package)

(unless (or (daemonp) noninteractive)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    ;; If `file-name-handler-alist' is nil, no 256 colors in TUI
    ;; @see https://emacs-china.org/t/spacemacs-petmacs-emacs/3802/839
    (setq file-name-handler-alist
          (unless (display-graphic-p)
            '(("\\(?:\\.tzst\\|\\.zst\\|\\.dz\\|\\.txz\\|\\.xz\\|\\.lzma\\|\\.lz\\|\\.g?z\\|\\.\\(?:tgz\\|svgz\\|sifz\\)\\|\\.tbz2?\\|\\.bz2\\|\\.Z\\)\\(?:~\\|\\.~[-[:alnum:]:#@^._]+\\(?:~[[:digit:]]+\\)?~\\)?\\'" . jka-compr-handler))))
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist
                                           old-file-name-handler-alist)))))))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 800000
                  gc-cons-percentage 0.1)))

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

(setq comp-deferred-compilation t)

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

;; ignore warnings
;;(setq warning-minimum-level :emergency)

(require 'init-custom)
(require 'init-const)
(require 'init-package)

(require 'init-basic)

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
;; (require 'init-ctags)

(require 'init-elisp)
(require 'init-c-c++)
(require 'init-python)
(require 'init-java)
(require 'init-scala)
(require 'init-golang)
(require 'init-web)
(require 'init-sql)

(require 'init-org)
(require 'init-markdown)
(require 'init-yaml)

(require 'init-shell)
(require 'init-misc)

(require 'core-funcs)
(require 'init-leader)

;;; init.el ends here
