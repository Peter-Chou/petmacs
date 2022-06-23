;; -*- lexical-binding: t no-byte-compile: t -*-

;; sudo apt install python3-pip
;; /usr/bin/python3 -m pip install pip setuptools --upgrade
;; python3 install install-eaf.py
(use-package eaf
  :load-path (lambda () (expand-file-name "site-lisp/emacs-application-framework" user-emacs-directory))
  :custom
  ;; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  :init
  ;; add folders in app to load-path
  (let ((default-directory (expand-file-name "site-lisp/emacs-application-framework/app" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path))
  (require 'eaf)
  (require 'eaf-browser)
  (require 'eaf-image-viewer)
  (require 'eaf-jupyter)
  (require 'eaf-markdown-previewer)
  (require 'eaf-org-previewer)
  (require 'eaf-pdf-viewer)

  (require 'eaf-all-the-icons)
  (require 'eaf-evil)

  (setq eaf-python-command (expand-file-name "eaf/bin/python" (getenv "WORKON_HOME")))
  :config
  (defalias 'browse-web #'eaf-open-browser)
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  )

(provide 'init-eaf)
