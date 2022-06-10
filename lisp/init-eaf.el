;; -*- lexical-binding: t -*-

;; sudo apt install python3-pip
;; /usr/bin/python3 -m pip install pip setuptools --upgrade
;; python3 install install-eaf.py
(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  :init
  (require 'eaf)
  (require 'eaf-browser)
  (require 'eaf-markdown-previewer)
  (require 'eaf-evil)
  )

(provide 'init-eaf)
