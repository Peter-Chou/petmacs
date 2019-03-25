;; init-project.el --- Setup project related packages.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package projectile
  :diminish
  :bind (:map projectile-mode-map
              ("s-t" . projectile-find-file) ; `cmd-t' or `super-t'
              ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix "")
  (setq projectile-sort-order 'recentf)
  (setq projectile-use-git-grep t)
  :config
  ;; (projectile-update-mode-line)         ; Update mode-line at the first time

  ;; Use the faster searcher to handle project files:
  ;; ripgrep `rg'
  (let ((command
         (when
             (executable-find "rg")
           (let ((rg-cmd ""))
             (dolist (dir projectile-globally-ignored-directories)
               (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
             (concat "rg -0 --files --color=never --hidden" rg-cmd)))
         ))
    (setq projectile-generic-command command))
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching nil)
  ;; FIXME: too slow while getting submodule files on Windows
  (setq projectile-git-submodule-command nil)
  )

(provide 'init-project)

;;; init-project.el ends here
