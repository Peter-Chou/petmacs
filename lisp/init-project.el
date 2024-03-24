;; -*- lexical-binding: t no-byte-compile: t -*-

(use-package project
  :preface
  (defun my/project-try-local (dir)
    "Determine if DIR is a non-Git project."
    (catch 'ret
      (let ((pr-flags '(
                        ;; (".project")
                        (".prj" ".project" ".projectile")
                        ("go.mod" "Cargo.toml" "project.clj" "pom.xml" "package.json") ;; higher priority
                        ("Makefile" "README.org" "README.md"))))
        (dolist (current-level pr-flags)
          (dolist (f current-level)
            (when-let ((root (locate-dominating-file dir f)))
              (throw 'ret (cons 'local root))))))))
  (defun my/project-files-in-directory (dir)
    "Use `fd' to list files in DIR."
    (let* ((default-directory dir)
           (localdir (file-local-name (expand-file-name dir)))
           (command (format "fd -H -t f -0 . %s" localdir)))
      (project--remote-file-names
       (sort (split-string (shell-command-to-string command) "\0" t)
             #'string<))))
  (defun my/project-info ()
    (interactive)
    (message "%s" (project-current t)))

  (defun my/add-dot-project ()
    (interactive)
    (let* ((root-dir (read-directory-name "Root: "))
           (f (expand-file-name ".project" root-dir)))
      (message "Create %s..." f)
      (make-empty-file f)))

  (defun my/project-discover ()
    "Add dir under search-path to project."
    (interactive)
    (dolist (search-path '("~/code/" "~/git/"))
      (dolist (file (file-name-all-completions  "" search-path))
        (when (not (member file '("./" "../")))
          (let ((full-name (expand-file-name file search-path)))
            (when (file-directory-p full-name)
              (when-let ((pr (project-current nil full-name)))
                (project-remember-project pr)
                (message "add project %s..." pr))))))))
  :ensure nil
  :init
  (setq project-find-functions '(my/project-try-local project-try-vc))
  :config
  (cl-defmethod project-files ((project (head local)) &optional dirs)
    "Override `project-files' to use `fd' in local projects."
    (mapcan #'my/project-files-in-directory
            (or dirs (list (project-root project))))))

(use-package consult-project-extra)

(provide 'init-project)
