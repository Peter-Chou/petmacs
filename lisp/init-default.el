;; init-default.el --- Setup defaults.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-variable))

;; use exec-path-from-shell in linux / mac
(when (or (eq system-type 'gnu/linux) (eq system-type 'darwin))
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-check-startup-files nil)
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH"))
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

;; maximize emacs after initialization
(toggle-frame-maximized)

;; use UTF-8 as default encoding
(set-language-environment petmacs-default-language-env)
(set-default-coding-systems petmacs-default-coding-env)

(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq visible-bell t)

(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder
(setq make-backup-files nil)               ; Forbide to make backup files
(setq auto-save-default nil)               ; Disable auto save
(setq create-lockfiles nil)                ; Disable lock files .#filename

(setq-default major-mode 'text-mode)

(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

;; Line and Column
(setq-default fill-column 80)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq column-number-mode t)
(setq line-number-mode t)

;; Don't open a file in a new frame
(when (boundp 'ns-pop-up-frames)
  (setq ns-pop-up-frames nil))

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;; (setq mouse-wheel-progressive-speed nil)
;; (setq scroll-step 1
;;       scroll-margin 0
;;       scroll-conservatively 100000)

;; Basic modes

;; Start server
(use-package server
  :ensure nil
  :hook (after-init . server-mode))

;; History
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :init
  (setq recentf-max-saved-items 200)
  (setq recentf-exclude '((expand-file-name package-user-dir)
                          ".cache"
                          ".cask"
                          ".elfeed"
                          "bookmarks"
                          "cache"
                          "ido.*"
                          "persp-confs"
                          "recentf"
                          "url"
                          "COMMIT_EDITMSG\\'")))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
					      global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

;; show search match information
(use-package anzu
  :init
  (global-anzu-mode t))

(provide 'init-default)

;;; init-default.el ends here
