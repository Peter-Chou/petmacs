;; init-evil.el --- Setup Evil.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-variable))

(use-package evil-leader
  :defer nil
  :init
  (global-evil-leader-mode))

(use-package evil-major-leader
  :quelpa
  (evil-major-leader :repo "Peter-Chou/evil-major-leader" :fetcher github)
  :init
  (global-evil-major-leader-mode))

(use-package evil-anzu)

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil) ;; use evil-collection instead
  (when evil-want-C-u-scroll
    (define-key evil-insert-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up))
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
    ;; Overload shifts so that they don't lose the selection
    (define-key evil-visual-state-map (kbd "<") 'petmacs//evil-visual-shift-left)
    (define-key evil-visual-state-map (kbd ">") 'petmacs//evil-visual-shift-right)))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; (use-package evil-magit
;;   :hook (magit-mode . evil-magit-init))

(use-package evil-escape
  :init
  (setq-default evil-escape-delay 0.3)
  (evil-escape-mode))

(use-package evil-commentary
  :hook (after-init . evil-commentary-mode))

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

;; using outline-minor-mode for evil folding
(use-package outline-mode
  :ensure nil
  :hook (prog-mode . outline-minor-mode)
  :init
  ;; (evil-define-key 'normal outline-mode-map (kbd "zK") 'outline-show-branches) ; Show all children recursively but no body.
  ;; (evil-define-key 'normal outline-mode-map (kbd "zk") 'outline-show-children) ; Direct children only unlike `outline-show-branches'
  (define-key evil-normal-state-map (kbd "zB") 'outline-hide-body) ; Hide all bodies
  (define-key evil-normal-state-map (kbd "zb") 'outline-show-all)  ; Hide current body
  (define-key evil-normal-state-map (kbd "ze") 'outline-show-entry) ; Show current body only, not subtree, reverse of outline-hide-entry
  (define-key evil-normal-state-map (kbd "zl") 'outline-hide-leaves) ; Like `outline-hide-body' but for current subtree only
  (define-key evil-normal-state-map (kbd "zp") 'outline-hide-other)    ; Hide all nodes and bodies except current body.
  (define-key evil-normal-state-map (kbd "zj") 'outline-forward-same-level)
  (define-key evil-normal-state-map (kbd "zk") 'outline-backward-same-level)
  (define-key evil-normal-state-map (kbd "M-j") 'outline-move-subtree-down)
  (define-key evil-normal-state-map (kbd "M-k") 'outline-move-subtree-up))

(provide 'init-evil)

;;; init-evil.el ends here
