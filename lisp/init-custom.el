;; init-custom.el --- Setup custom.el.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defvar petmacs-default-language-env 'UTF-8)

(defvar petmacs-default-coding-env 'utf-8)

(defvar petmacs-proxy "127.0.0.1:1080"
  "Set network proxy.")

;; (defvar petmacs-day-theme 'doom-solarized-light)
;; (defvar petmacs-night-theme 'doom-dracula)

(defvar petmacs-themes-list '(
			      doom-snazzy
			      doom-tomorrow-day
			      doom-solarized-light
			      doom-solarized-dark
			      doom-one-light
			      doom-dracula
			      )
  "List of themes for cycling.")

(defvar petmacs-font-size 12.5
  "Default font size.")

(defvar petmacs-evil-leader-key "<SPC>"
  "Evil leader key.")

(defvar petmacs-evil-major-leader-key "\,"
  "Evil major leader key.")

(defvar petmacs-evil-major-leader-insert-default-key "M-m"
  "Evil leader key in evil insert mode.")

(defvar petmacs-lsp-active-modes '(
				   c-mode
				   c++-mode
				   python-mode
				   java-mode
				   scala-mode
				   go-mode
				   sh-mode
				   )
  "Primary major modes of the lsp activated layer.")

(defvar org-projectile-file "TODOs.org"
  "The file to store project TODOs in. If this is a relative
path, one file per project is used (and the path is relative to
the project root). If it an absolute path, one global file is
used.")


(defvar petmacs--default-theme (pop petmacs-themes-list)
  "petmacs default theme as the first theme in petmcas-themes-list")

(defvar petmacs-default-mode-for-headers 'c++-mode
  "default default mode for .h header files, Can be `c-mode' or `c++-mode'")

(defvar python-shell--interpreter nil)
(defvar python-shell--interpreter-args nil)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(let ((custom-template-file
       (expand-file-name "custom-template.el" user-emacs-directory)))
  (if (not (file-exists-p custom-file))
      (copy-file custom-template-file custom-file)))

;; load custom-set-variables & custom-set-faces in custom file
(load-file custom-file)

(provide 'init-custom)

;;; init-custom.el ends here
