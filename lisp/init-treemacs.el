;; init-treemacs.el --- Setup treemacs.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package treemacs
  ;; :pin melpa-stable
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
			   (hl-line-mode -1)))
  :init
  (define-key winum-keymap (kbd "M-0") 'treemacs-select-window)
  :config
  (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
        treemacs-sorting                       'alphabetic-case-insensitive-desc
        treemacs-follow-after-init             t
        treemacs-width                         30)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  ;; (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'extended))
    (`(t . _)
     (treemacs-git-mode 'simple))))

(use-package treemacs-evil
  ;; :pin melpa-stable
  :defer t
  :init
  (with-eval-after-load 'treemacs
    (require 'treemacs-evil))
  :config
  (define-key evil-treemacs-state-map (kbd "F") 'treemacs-create-file)
  (define-key evil-treemacs-state-map (kbd "+") 'treemacs-create-dir))

(use-package treemacs-projectile
  ;; :pin melpa-stable
  :after treemacs projectile
  :bind (([M-f8] . treemacs-projectile)
	 :map projectile-command-map
	 ("h" . treemacs-projectile)))

(use-package treemacs-magit
  ;; :pin melpa-stable
  :after treemacs magit
  :commands treemacs-magit--schedule-update
  :hook ((magit-post-commit
          git-commit-post-finish
          magit-post-stage
          magit-post-unstage)
         . treemacs-magit--schedule-update))

(use-package treemacs-persp
  :after persp-mode
  :demand t
  :functions treemacs-set-scope-type
  :config (treemacs-set-scope-type 'Perspectives))

(provide 'init-treemacs)

;;; init-treemacs.el ends here
