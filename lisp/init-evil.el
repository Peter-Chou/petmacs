;; init-evil.el --- Setup Evil.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(use-package evil-anzu)

(use-package evil
  ;; :pin melpa-stable
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil) ;; use evil-collection instead
  :config
  (require 'evil-anzu)
  (evil-mode 1)
  (progn
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
    ;; treat _ as word like vim
    (with-eval-after-load 'evil
      (defalias #'forward-evil-word #'forward-evil-symbol))

    (when evil-want-C-u-scroll
      (define-key evil-insert-state-map (kbd "C-u") 'evil-scroll-up)
      (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
      (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
      (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up))
    ;; Overload shifts so that they don't lose the selection
    (define-key evil-visual-state-map (kbd "<") 'petmacs//evil-visual-shift-left)
    (define-key evil-visual-state-map (kbd ">") 'petmacs//evil-visual-shift-right)
    ))

(use-package evil-escape
  :init
  (setq-default evil-escape-delay 0.3)
  (evil-escape-mode))

(use-package evil-nerd-commenter
  :init
  (evil-define-key 'normal prog-mode-map
    "gc" 'evilnc-comment-or-uncomment-lines
    "gy" 'evilnc-comment-and-kill-ring-save))

(use-package evil-surround
  :init
  (global-evil-surround-mode 1))

(use-package evil-visualstar
  :commands (evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (progn
    (define-key evil-visual-state-map (kbd "*")
      'evil-visualstar/begin-search-forward)
    (define-key evil-visual-state-map (kbd "#")
      'evil-visualstar/begin-search-backward)))

(use-package evil-fringe-mark
  :ensure t
  :config
  (setq-default right-fringe-width 25)
  (setq-default evil-fringe-mark-side 'right-fringe)
  (global-evil-fringe-mark-mode))

(use-package evil-iedit-state
  :commands (evil-iedit-state evil-iedit-state/iedit-mode)
  :init
  (setq iedit-current-symbol-default t
        iedit-only-at-symbol-boundaries t
        iedit-toggle-key-default nil)
  :config
  ;; set TAB action
  (define-key iedit-occurrence-keymap-default (kbd "TAB") 'iedit-toggle-selection)
  (define-key iedit-occurrence-keymap-default [tab] 'iedit-toggle-selection))

(use-package bind-map)

;; (use-package evil-collection
;;   :after evil
;;   :custom (evil-collection-setup-minibuffer t)
;;   :config
;;   ;; (with-eval-after-load 'dired (evil-collection-dired-setup))
;;   (with-eval-after-load 'ibuffer (evil-collection-ibuffer-setup))
;;   (with-eval-after-load 'magit (evil-collection-magit-setup))
;;   (with-eval-after-load 'calendar (evil-collection-calendar-setup))
;;   (with-eval-after-load 'which-key (evil-collection-which-key-setup))
;;   (with-eval-after-load 'imenu-list (evil-collection-imenu-list-setup))
;;   (with-eval-after-load 'lsp-ui-imenu (evil-collection-lsp-ui-imenu-setup))
;;   (with-eval-after-load 'flycheck (evil-collection-flycheck-setup))
;;   (with-eval-after-load 'eshell (evil-collection-eshell-setup))
;;   (with-eval-after-load 'docker (evil-collection-docker-setup))
;;   (with-eval-after-load 'diff-mode (evil-collection-diff-mode-setup))
;;   (with-eval-after-load 'ediff (evil-collection-ediff-setup))
;;   (with-eval-after-load 'comint (evil-collection-comint-setup))
;;   (with-eval-after-load 'company (evil-collection-company-setup))
;;   (with-eval-after-load 'cmake-mode (evil-collection-cmake-mode-setup))
;;   )

(provide 'init-evil)

;;; init-evil.el ends here
