;; init-treemacs.el --- Initialize treemacs.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Treemacs: A tree layout file explorer.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

;; A tree layout file explorer
(use-package treemacs
  :functions (treemacs-follow-mode
              treemacs-filewatch-mode
              treemacs-git-mode
              treemacs-set-scope-type)
  :custom-face
  (cfrs-border-color ((t (:inherit posframe-border))))
  :config
  (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
        treemacs-missing-project-action  'remove
        treemacs-user-mode-line-format   'none
        treemacs-sorting                 'alphabetic-asc
        treemacs-follow-after-init       t
        treemacs-no-png-images           (not petmacs-icon)
        treemacs-width                   (if (petmacs/ultra-screen-p)
                                             petmacs-ultra-sidebar-width
                                           petmacs-sidebar-width))
  :config
  (add-hook 'treemacs-mode-hook
            (lambda ()
              (text-scale-set -0.5)))

  ;; (with-eval-after-load 'golden-ratio
  ;;   (add-to-list 'golden-ratio-exclude-buffer-regexp
  ;;                (rx "*Treemacs" (0+ any))))

  (treemacs-follow-mode t)
  (treemacs-project-follow-mode t) ;; treemacs just show current project tree
  (treemacs-filewatch-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))

(use-package treemacs-nerd-icons
  :demand t
  :autoload treemacs-nerd-icons-config
  :init (treemacs-nerd-icons-config))

(use-package treemacs-projectile
  :after projectile
  :bind (:map projectile-command-map
	     ("h" . treemacs-projectile)))

(use-package treemacs-magit
  :demand t
  :after magit)

(use-package treemacs-tab-bar
  :demand t
  :config (treemacs-set-scope-type 'Tabs))

(provide 'init-treemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-treemacs.el ends here
