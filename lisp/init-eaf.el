;; -*- lexical-binding: t no-byte-compile: t -*-

(require 'init-funcs)

;; sudo apt install python3-pip
;; /usr/bin/python3 -m pip install pip setuptools --upgrade
;; python3 install install-eaf.py
(use-package eaf
  :load-path (lambda () (expand-file-name "site-lisp/emacs-application-framework" user-emacs-directory))
  :init
  (setq eaf-config-location  (expand-file-name "data/eaf-config" user-emacs-directory)
        eaf-browser-continue-where-left-off t
        eaf-browser-enable-adblocker t
        browse-url-browser-function 'eaf-open-browser)
  ;; add folders in app to load-path
  ;; (let ((default-directory (petmacs/get-eaf-app-directory)))
  ;;   (normal-top-level-add-subdirs-to-load-path)

  (add-to-list 'load-path (concat (petmacs/get-eaf-app-directory) "browser"))
  (add-to-list 'load-path (concat (petmacs/get-eaf-app-directory) "image-viewer"))
  (add-to-list 'load-path (concat (petmacs/get-eaf-app-directory) "jupyter"))
  (add-to-list 'load-path (concat (petmacs/get-eaf-app-directory) "markdown-previewer"))
  (add-to-list 'load-path (concat (petmacs/get-eaf-app-directory) "org-previewer"))
  (add-to-list 'load-path (concat (petmacs/get-eaf-app-directory) "pdf-viewer"))
  (add-to-list 'load-path (concat (petmacs/get-eaf-app-directory) "git"))
  (add-to-list 'load-path (expand-file-name "site-lisp/emacs-application-framework/extension" user-emacs-directory))

  (require 'eaf)
  (require 'eaf-browser)
  (require 'eaf-image-viewer)
  (require 'eaf-jupyter)
  (require 'eaf-markdown-previewer)
  (require 'eaf-org-previewer)
  (require 'eaf-pdf-viewer)
  (require 'eaf-git)

  ;; (require 'eaf-all-the-icons)
  (require 'eaf-evil)

  (setq eaf-python-command (expand-file-name "eaf/bin/python" (getenv "WORKON_HOME")))
  ; )
  :config
  (with-eval-after-load 'spaceleader
    (setq eaf-evil-leader-key leader-key
          eaf-evil-leader-keymap leader-map))

  (defalias 'browse-web #'eaf-open-browser)
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding))

(provide 'init-eaf)
