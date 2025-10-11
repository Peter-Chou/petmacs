;; -*- lexical-binding: t no-byte-compile: t -*-

(eval-when-compile
  (require 'init-funcs))

(setq project-find-functions '(petmacs/project-try-local project-try-vc))

(use-package consult-project-extra)

(use-package projectile
  :diminish
  :pretty-hydra
  ((:title (pretty-hydra-title "project" 'octicon "nf-oct-project_roadmap")
    :foreign-keys warn :color blue :quit-key ("q" "C-g"))
   ("core"
    (("a" projectile-add-known-project "add project")
     ("x" projectile-remove-known-project "remove project")
     ("p" consult-projectile-switch-project "switch project")
     ("r" consult-projectile-recentf "recent files")
     ("-" projectile-dired "dired on root")
     ("'" multi-vterm-project "shell on root")
     ("d" consult-projectile-find-dir "open directory")
     ("e" projectile-edit-dir-locals "edit .dir-locals.el")
     ("I" projectile-invalidate-cache "clean invalid cache"))
    "file"
    (("ff" consult-projectile-find-file "find file")
     ("fF" consult-projectile-find-file-other-frame "find file other frame")
     ("fo" consult-projectile-find-file-other-window "find file other window"))
    "buffer"
    (("bb" consult-projectile-switch-to-buffer "switch buffer")
     ("bF" consult-projectile-switch-to-buffer-other-frame "switch buffer other frame")
     ("bo" 'consult-projectile-switch-to-buffer-other-window "switch buffer other window"))
    "org"
    (("oo" org-projectile/goto-project-todos "go to todos")
     ("oc" org-projectile-project-todo-completing-read "todo completion"))))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix ""
	    ;; projectile-enable-caching t
	    ;; projectile-indexing-method 'native
	    projectile-sort-order 'recentf
	    projectile-use-git-grep t)
  :config
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

(use-package consult-projectile
  :after consult
  :init (setq consult-projectile-use-projectile-switch-project t))

(provide 'init-project)
