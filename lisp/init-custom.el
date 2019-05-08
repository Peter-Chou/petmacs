;; init-custom.el --- Setup custom.el.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defvar petmacs-default-language-env 'UTF-8)

(defvar petmacs-default-coding-env 'utf-8)

(defvar petmacs-font-size 12.5
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

(defvar python-shell--interpreter nil)
(defvar python-shell--interpreter-args nil)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; load custom-set-variables & custom-set-faces in custom file
(load-file custom-file)

(provide 'init-custom)

;;; init-custom.el ends here
