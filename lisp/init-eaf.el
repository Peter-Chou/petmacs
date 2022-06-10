;; -*- lexical-binding: t -*-

;; sudo apt install python3-pip
;; /usr/bin/python3 -m pip install pip setuptools --upgrade
;; python3 install install-eaf.py
(use-package eaf
  ;; :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  :load-path (lambda () (expand-file-name "site-lisp/emacs-application-framework" user-emacs-directory))
  :init
  (let ((default-directory (expand-file-name "site-lisp/emacs-application-framework/app" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path))
  (require 'eaf)
  (require 'eaf-jupyter)
  (require 'eaf-markdown-previewer)
  (require 'eaf-pdf-viewer)
  (require 'eaf-image-viewer)
  (require 'eaf-org-previewer)
  (require 'eaf-browser)

  (require 'eaf-all-the-icons)
  (require 'eaf-evil)

  (setq eaf-python-command (expand-file-name "eaf/bin/python" (getenv "WORKON_HOME"))))

(provide 'init-eaf)
