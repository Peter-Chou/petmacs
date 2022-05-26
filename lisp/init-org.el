;; -*- lexical-binding: t -*-

(use-package org
  :ensure nil
  :custom-face (org-ellipsis ((t (:foreground nil))))
  :hook ((org-babel-after-execute org-mode) . org-redisplay-inline-images) ; display image
  :config
  (setq org-modules nil                 ; Faster loading
        org-directory (expand-file-name "org" user-emacs-directory)

        ;; Agenda styling
        org-agenda-files (list (expand-file-name "org" user-emacs-directory))
        org-agenda-block-separator ?─
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "⭠ now ─────────────────────────────────────────────────"

        org-tags-column -80
        org-log-done 'time
        org-catch-invisible-edits 'smart
        org-startup-indented t
        org-ellipsis (if (char-displayable-p ?⏷) "\t⏷" nil)
        org-pretty-entities nil
        org-hide-emphasis-markers t))

(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)
         (org-modern-mode . (lambda ()
                              "Adapt `org-modern-mode'."
                              ;; Disable Prettify Symbols mode
                              (setq prettify-symbols-alist nil)
                              (prettify-symbols-mode -1)))))

(use-package org-superstar
  :if (and (display-graphic-p) (char-displayable-p ?◉))
  :hook (org-mode . org-superstar-mode)
  :init (setq org-superstar-headline-bullets-list '("◉""○""◈""◇""⁕")))

(use-package org-fancy-priorities
  :diminish
  :hook (org-mode . org-fancy-priorities-mode)
  :init (setq org-fancy-priorities-list
              (if (and (display-graphic-p) (char-displayable-p ?🅐))
                  '("🅐" "🅑" "🅒" "🅓")
                '("HIGH" "MEDIUM" "LOW" "OPTIONAL"))))

(use-package org-projectile
  :commands (org-projectile-location-for-project
             org-projectile-todo-files)
  :preface
  (defun org-projectile/goto-project-todos ()
    (interactive)
    (org-projectile-goto-location-for-project (projectile-project-name))
    (revert-buffer))
  :init

  (setq org-projectile-projects-file (expand-file-name "todos.org" (concat user-emacs-directory "org"))
        org-agenda-files (append org-agenda-files (org-projectile-todo-files))
        org-projectile-per-project-filepath "todos.org")

  (unless (file-exists-p org-projectile-projects-file)
    (write-region "" "" org-projectile-projects-file))

  (with-eval-after-load 'org-capture
    (require 'org-projectile)))


(provide 'init-org)
