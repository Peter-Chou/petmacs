
(use-package evil-leader
  :defer nil
  :init
  (global-evil-leader-mode)
  )

(use-package evil-anzu)

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)

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

(use-package evil-escape
  :init
  (progn
    (setq-default evil-escape-delay 0.3)
    (evil-escape-mode)))

(use-package evil-nerd-commenter
  :commands evilnc-comment-operator
  :init
  (progn
    (define-key evil-normal-state-map "gc" 'evilnc-comment-operator)))

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
  (progn
    (setq-default right-fringe-width 25)
    (setq-default evil-fringe-mark-side 'right-fringe)
    (global-evil-fringe-mark-mode)))

(provide 'init-evil)
