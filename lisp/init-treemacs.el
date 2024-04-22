;; -*- lexical-binding: t no-byte-compile: t -*-

;; A tree layout file explorer
(use-package treemacs
  :commands (
             treemacs-select-window
             treemacs-select-scope-type
             treemacs--window-number-ten
             treemacs-current-visibility
             treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-git-mode)
  :custom-face
  (cfrs-border-color ((t (:inherit posframe-border))))
  :bind (:map treemacs-mode-map
         ([mouse-1]   . treemacs-single-click-expand-action))
  :config
  (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
        treemacs-missing-project-action  'remove
        treemacs-sorting                 'alphabetic-asc
        treemacs-is-never-other-window   t
        treemacs-follow-after-init       t
        treemacs-no-png-images           (not petmacs-icon)
        treemacs-width                   25)
  :config
  (with-eval-after-load 'golden-ratio
    (add-to-list 'golden-ratio-exclude-buffer-regexp
                 (rx "*Treemacs" (0+ any))))

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))

(use-package treemacs-nerd-icons
  :demand t
  :when (icons-displayable-p)
  :custom-face
  (treemacs-nerd-icons-root-face ((t (:inherit nerd-icons-green :height 1.3))))
  (treemacs-nerd-icons-file-face ((t (:inherit nerd-icons-dsilver))))
  :config (treemacs-load-theme "nerd-icons"))

(use-package treemacs-projectile
  :after projectile
  :bind (:map projectile-command-map
	     ("h" . treemacs-projectile)))

(use-package treemacs-magit
  :after magit
  :commands treemacs-magit--schedule-update
  :hook ((magit-post-commit
          git-commit-post-finish
          magit-post-stage
          magit-post-unstage)
         . treemacs-magit--schedule-update))

(use-package treemacs-tab-bar
  :demand t
  :config (treemacs-set-scope-type 'Tabs))

(provide 'init-treemacs)
