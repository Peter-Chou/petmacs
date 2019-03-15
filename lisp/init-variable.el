;; init-variable.el --- Define petmacs vaiables.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defvar petmacs-default-language-env 'UTF-8)

(defvar petmacs-default-coding-env 'utf-8)

(defvar petmacs--font-size 17.5
  "default font size")

(defvar petmacs-evil-leader-key "<SPC>"
  "Evil leader key.")

(defvar petmacs-evil-major-leader-key "\,"
  "Evil major leader key.")

(defvar org-projectile-file "TODOs.org"
  "The file to store project TODOs in. If this is a relative
path, one file per project is used (and the path is relative to
the project root). If it an absolute path, one global file is
used.")


(provide 'init-variable)

;;; init-const.el ends here
