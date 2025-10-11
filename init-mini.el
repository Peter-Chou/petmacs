;; init-mini.el --- Petmacs minimal configurations -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:
;;
;; Minimal configurations for debugging purpose.
;;

;;; Code:

;; Load path
(push (expand-file-name "site-lisp" user-emacs-directory) load-path)
(push (expand-file-name "lisp" user-emacs-directory) load-path)

;; Packages
;; Without this comment Emacs25 adds (package-initialize) here
(setq package-archives '(
                         ;; ("gnu"       . "https://elpa.gnu.org/packages/")
                         ;; ("gnu-devel" . "https://elpa.gnu.org/devel/")
                         ;; ("melpa"     . "https://melpa.org/packages/")
                         ;; ("nongnu"    . "https://elpa.nongnu.org/nongnu/")
                         ;; ("org"       . "https://orgmode.org/elpa/")

                         ;;; emacs-china mirror
                         ;; ("gnu"    . "http://1.15.88.122/gnu/")
                         ;; ("melpa"  . "http://1.15.88.122/melpa/")
                         ;; ("nongnu" . "http://1.15.88.122/nongnu/")
                         ;; ("org"    . "http://1.15.88.122/org/")

                         ;;; tsinghua mirror
                         ("gnu"       . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("gnu-devel" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu-devel/")
                         ("melpa"     . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("nongnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("org"       . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
                         ))

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;; Better defaults
;; (setq initial-scratch-message nil)
(setq inhibit-splash-screen t)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder
(setq make-backup-files nil)               ; Forbide to make backup files
(setq auto-save-default nil)               ; Disable auto save
(setq set-mark-command-repeat-pop t)       ; Repeating C-SPC after popping mark pops it again
;; (setq kill-whole-line t)                   ; Kill line including '\n'

(setq-default major-mode 'text-mode)

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset   4
              tab-width        4
              indent-tabs-mode nil)

;; UI
(load-theme 'wombat t)

(unless (eq window-system 'ns)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; (global-hl-line-mode 1)

;; (if (fboundp 'display-line-numbers-mode)
;;     (global-display-line-numbers-mode 1)
;;   (global-linum-mode 1))

;; Basic modes
(show-paren-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(recentf-mode 1)
(when (fboundp 'savehist-mode)
  (savehist-mode 1))
(if (fboundp 'save-place-mode)
    (save-place-mode 1)
  (require 'saveplace)
  (setq-default save-place t))

(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode 1)

(add-hook 'prog-mode-hook #'subword-mode)
(add-hook 'minibuffer-setup-hook #'subword-mode)

;; Completion
(when (fboundp 'completion-preview-mode)
  (completion-preview-mode 1))

(if (fboundp 'fido-mode)
    (progn
      (fido-mode 1)
      (when (fboundp 'fido-vertical-mode)
        (fido-vertical-mode 1))

      (defun fido-recentf-open ()
        "Use `completing-read' to find a recent file."
        (interactive)
        (if (find-file (completing-read "Find recent file: " recentf-list))
            (message "Opening file...")
          (message "Aborting")))
      (global-set-key (kbd "C-x C-r") 'fido-recentf-open))
  (progn
    (ido-mode 1)
    (ido-everywhere 1)

    (setq ido-use-virtual-buffers t
          ido-use-filename-at-point 'guess
          ido-create-new-buffer 'always
          ido-enable-flex-matching t)

    (defun ido-recentf-open ()
      "Use `ido-completing-read' to find a recent file."
      (interactive)
      (if (find-file (ido-completing-read "Find recent file: " recentf-list))
          (message "Opening file...")
        (message "Aborting")))
    (global-set-key (kbd "C-x C-r") 'ido-recentf-open)))

;; Key Modifiers
(cond
 ((eq system-type 'windows-nt)
  ;; make PC keyboard's Win key or other to type Super or Hyper
  ;; (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super     ; Left Windows key
        w32-apps-modifier 'hyper)       ; Menu/App key
  (w32-register-hot-key [s-t]))
 ((eq window-system 'mac)
  ;; Compatible with Emacs Mac port
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super)
  (global-set-key [(super a)] #'mark-whole-buffer)
  (global-set-key [(super v)] #'yank)
  (global-set-key [(super c)] #'kill-ring-save)
  (global-set-key [(super s)] #'save-buffer)
  (global-set-key [(super l)] #'goto-line)
  (global-set-key [(super w)] #'delete-frame)
  (global-set-key [(super z)] #'undo)))

;; Keybindings
(global-set-key (kbd "<C-return>") #'rectangle-mark-mode)

(defun revert-current-buffer ()
  "Revert the current buffer."
  (interactive)
  (message "Revert this buffer")
  (text-scale-set 0)
  (widen)
  (revert-buffer t t))
(global-set-key (kbd "<f5>") #'revert-current-buffer)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-x") #'ielm)
            (local-set-key (kbd "C-c C-c") #'eval-defun)
            (local-set-key (kbd "C-c C-b") #'eval-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Init-mini.el ends here
