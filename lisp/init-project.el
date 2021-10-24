;; init-project.el --- Setup project related packages.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(use-package projectile
  :diminish
  :bind (:map projectile-mode-map
         ("s-t" . projectile-find-file) ; `cmd-t' or `super-t'
         ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix ""
	projectile-sort-order 'recentf
	projectile-use-git-grep t
	projectile-enable-caching t
	projectile-indexing-method 'native
	)
  :config
  ;; (projectile-update-mode-line)         ; Update mode-line at the first time

  ;; Use the faster searcher to handle project files: ripgrep `rg'.
  (when (executable-find "rg")
    (setq projectile-generic-command
          (let ((rg-cmd ""))
            (dolist (dir projectile-globally-ignored-directories)
              (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
            (concat "rg -0 --files --color=never --hidden" rg-cmd))))

  ;; Faster searching on Windows
  (when sys/win32p
    (when (executable-find "rg")
      (setq projectile-indexing-method 'alien)
      (setq projectile-enable-caching nil))

    ;; FIXME: too slow while getting submodule files on Windows
    (setq projectile-git-submodule-command nil)))

(provide 'init-project)

;;; init-project.el ends here
