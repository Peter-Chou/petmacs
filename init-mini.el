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
                         ("gnu"       . "https://elpa.gnu.org/packages/")
                         ("gnu-devel" . "https://elpa.gnu.org/devel/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("nongnu"    . "https://elpa.nongnu.org/nongnu/")
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

(global-hl-line-mode 1)
(global-display-line-numbers-mode 1)

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


(use-package undo-fu)

(use-package evil
  :preface
  (defun petmacs//evil-visual-shift-left ()
    "evil left shift without losing selection"
    (interactive)
    (call-interactively 'evil-shift-left)
    (evil-normal-state)
    (evil-visual-restore))

  (defun petmacs//evil-visual-shift-right ()
    "evil right shift without losing selection"
    (interactive)
    (call-interactively 'evil-shift-right)
    (evil-normal-state)
    (evil-visual-restore))
  :init
  (setq evil-want-C-u-scroll t
	    evil-want-integration t
	    ;; `evil-want-C-i-jump' is set to nil to avoid `TAB' being
	    ;; overlapped in terminal mode. The GUI specific `<C-i>' is used
	    ;; instead.
	    evil-want-C-i-jump nil
	    evil-want-keybinding nil ;; use evil-collection instead
	    evil-overriding-maps nil)
  (evil-mode 1)
  :config
  (define-key evil-normal-state-map   (kbd "C-g") #'keyboard-quit)
  (define-key evil-motion-state-map   (kbd "C-g") #'keyboard-quit)
  (define-key evil-insert-state-map   (kbd "C-g") #'keyboard-quit)
  (define-key evil-window-map         (kbd "C-g") #'keyboard-quit)
  (define-key evil-operator-state-map (kbd "C-g") #'keyboard-quit)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (define-key evil-visual-state-map (kbd "y") 'petmacs/evil-yank)
  (define-key evil-normal-state-map (kbd "Y") 'petmacs/yank-to-end-of-line)

  (define-key evil-normal-state-map (kbd "g[") (lambda () (interactive) (beginning-of-defun)))
  (define-key evil-normal-state-map (kbd "g]") (lambda () (interactive) (end-of-defun)))

  (evil-set-undo-system 'undo-fu)

  ;; treat _ as word like vim
  (with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol))

  (with-eval-after-load 'eldoc
    (eldoc-add-command #'evil-cp-insert)
    (eldoc-add-command #'evil-cp-insert-at-end-of-form)
    (eldoc-add-command #'evil-cp-insert-at-beginning-of-form)
    (eldoc-add-command #'evil-cp-append))

  (when evil-want-C-u-scroll
    (define-key evil-insert-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up))

  ;; Overload shifts so that they don't lose the selection
  (define-key evil-visual-state-map (kbd "<") 'petmacs//evil-visual-shift-left)
  (define-key evil-visual-state-map (kbd ">") 'petmacs//evil-visual-shift-right))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-mini.el ends here
