;; init-org.el --- Setup org mode.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(eval-when-compile
  (require 'init-const)
  (require 'init-variable))

(use-package org
  :commands (orgtbl-mode)
  :init
  (require 'org)
  (setq org-directory "~/org"
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-log-done t
        org-startup-with-inline-images t
        org-image-actual-width nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        ;; this is consistent with the value of
        ;; `helm-org-headings-max-depth'.
        org-imenu-depth 8))

(use-package org-agenda
  :ensure nil
  :init
  (setq org-agenda-restore-windows-after-quit t)
  )

(use-package org-bullets
  :if (char-displayable-p ?◉)
  :hook (org-mode . org-bullets-mode))

(use-package toc-org
  :hook (org-mode . toc-org-enable)
  :init
  (setq toc-org-max-depth 10))

(use-package org-projectile
  :defer nil
  :commands (org-projectile-location-for-project)
  :preface
  (defun org-projectile/capture (&optional arg)
    (interactive "P")
    (if arg
	(org-projectile-project-todo-completing-read :empty-lines 1)
      (org-projectile-capture-for-current-project :empty-lines 1)))

  (defun org-projectile/goto-todos ()
    (interactive)
    (org-projectile-goto-location-for-project (projectile-project-name)))

  :init
  (with-eval-after-load 'org-capture
    (require 'org-projectile))

  :config
  (if (file-name-absolute-p org-projectile-file)
      (progn
        (setq org-projectile-projects-file org-projectile-file)
        (push (org-projectile-project-todo-entry :empty-lines 1)
              org-capture-templates))
    (org-projectile-per-project)
    (setq org-projectile-per-project-filepath org-projectile-file)))

(use-package org-plus-contrib)

(provide 'init-org)

;;; init-org.el ends here
