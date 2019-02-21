
(use-package treemacs
  :defines winum-keymap
  :commands (treemacs-follow-mode
	     treemacs-current-visibility
	     treemacs-select-window
	     treemacs--window-number-ten
             treemacs-filewatch-mode
             treemacs-fringe-indicator-mode
             treemacs-git-mode)
  :bind (([f8]        . treemacs)
         ("C-`"       . treemacs-select-window)
         ("M-0"       . treemacs-select-window)
         ("C-x 1"     . treemacs-delete-other-windows)
         ("C-x t 1"   . treemacs-delete-other-windows)
         ("C-x t t"   . treemacs)
         ("C-x t b"   . treemacs-bookmark)
         ("C-x t C-t" . treemacs-find-file)
         ("C-x t M-t" . treemacs-find-tag)
         :map treemacs-mode-map
         ([mouse-1]   . treemacs-single-click-expand-action))
  :hook (treemacs-mode . (lambda ()
			   (display-line-numbers-mode -1)
			   (hl-line-mode -1)
			   (vim-empty-lines-mode -1)))
  :init
  ;; (with-eval-after-load 'winum
  ;;   (bind-key (kbd "M-0") #'treemacs-select-window winum-keymap))
  (define-key winum-keymap (kbd "M-0") 'treemacs-select-window)
  :config
  (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
        treemacs-file-event-delay           5000
        treemacs-follow-after-init          t
        treemacs-follow-recenter-distance   0.1
        treemacs-goto-tag-strategy          'refetch-index
        treemacs-position 'left
        treemacs-indentation                2
        treemacs-indentation-string         " "
        treemacs-is-never-other-window      nil
        treemacs-no-png-images              nil
        treemacs-recenter-after-file-follow nil
        treemacs-recenter-after-tag-follow  nil
        treemacs-show-hidden-files          t
        treemacs-silent-filewatch           t
        treemacs-silent-refresh             t
        treemacs-sorting                    'alphabetic-desc
        treemacs-tag-follow-cleanup         t
        treemacs-tag-follow-delay           1.5
        treemacs-width                      30)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'extended))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  (if (fboundp 'define-fringe-bitmap)
      (define-fringe-bitmap 'treemacs--fringe-indicator-bitmap
        (vector #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111))))

;; (use-package treemacs-evil
;;   :defer nil
;;   :after treemacs)

(use-package treemacs-evil
  :defer t
  :init
  (with-eval-after-load 'treemacs
    (require 'treemacs-evil))
  :config
  (define-key evil-treemacs-state-map (kbd "F") 'treemacs-create-file)
  (define-key evil-treemacs-state-map (kbd "+") 'treemacs-create-dir))

(use-package treemacs-projectile
  :after treemacs)

(provide 'init-treemacs)
