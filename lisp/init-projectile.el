;; -*- lexical-binding: t no-byte-compile: t -*-

(use-package projectile
  :diminish
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix ""
	    projectile-sort-order 'recentf
	    projectile-use-git-grep t
	    ;; projectile-enable-caching t
	    ;; projectile-indexing-method 'native
        )
  :config
  ;; workaround of issue https://github.com/bbatsov/projectile/issues/1788
  (setq projectile-git-fd-args "-H -0 -E .git -tf")

  ;; (projectile-update-mode-line)         ; Update mode-line at the first time

  ;; Use the faster searcher to handle project files: ripgrep `rg'.
  (when (and (not (executable-find "fd"))
             (executable-find "rg"))
    (setq projectile-generic-command
          (let ((rg-cmd ""))
            (dolist (dir projectile-globally-ignored-directories)
              (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
            (concat "rg -0 --files --color=never --hidden" rg-cmd))))

  ;; Faster searching on Windows
  (when sys/win32p
    (when (or (executable-find "fd") (executable-find "rg"))
      (setq projectile-indexing-method 'alien
            projectile-enable-caching nil))

    ;; FIXME: too slow while getting submodule files on Windows
    (setq projectile-git-submodule-command nil))

  ;; Support Perforce project
  (let ((val (or (getenv "P4CONFIG") ".p4config")))
    (add-to-list 'projectile-project-root-files-bottom-up val)))

(provide 'init-projectile)
