;; -*- lexical-binding: t no-byte-compile: t -*-

(require 'init-custom)

(require'org-tempo) ;; start easy template

(use-package org
  :pin melpa
  :custom-face (org-ellipsis ((t (:foreground unspecified))))
  :hook (
         ((org-babel-after-execute org-mode) . org-redisplay-inline-images)) ; display image
  :init
  (make-directory (expand-file-name "data/gtd" user-emacs-directory) t)
  :config
  (setq org-modules nil                 ; Faster loading
        org-directory (expand-file-name "data/gtd" user-emacs-directory)

        org-todo-keywords
        '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")
          (sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)"))
        org-todo-keyword-faces '(("HANGUP" . warning)
                                 ("❓" . warning))
        org-priority-faces '((?A . error)
                             (?B . warning)
                             (?C . success))

        ;; Agenda styling
        org-agenda-files (list (expand-file-name "data/gtd" user-emacs-directory))
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
                              (prettify-symbols-mode -1))))
  :init (setq org-modern-star t))

(use-package org-superstar
  :if (and (display-graphic-p) (char-displayable-p ?◉))
  :hook (org-mode . org-superstar-mode)
  :init (setq org-superstar-headline-bullets-list '("◉""○""◈""◇""⁕")
              org-superstar-special-todo-items t))

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
    (revert-buffer t t))
  :init
  (setq org-projectile-per-project-filepath "TODO.org"
        org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  :config
  (with-eval-after-load 'org-capture
    (require 'org-projectile)
    (org-projectile-per-project))
  (push (org-projectile-project-todo-entry) org-capture-templates))

(use-package org-contrib
  :pin nongnu)

(use-package visual-fill-column
  :preface
  (defun petmacs/org-mode-visual-fill ()
    (setq visual-fill-column-width 100
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))
  :hook (org-mode . petmacs/org-mode-visual-fill))

(use-package org-super-agenda)

(use-package evil-org
  :hook (org-mode . evil-org-mode)
  :after org
  :config
  (setq evil-org-use-additional-insert t
        evil-org-key-theme '(textobjects
                             navigation
                             additional
                             todo)))

(use-package org-appear
  :preface
  (defun org-apperance-evil-hack ()
    (add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start nil t)
    (add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop nil t))
  :after org
  :hook (org-mode . org-appear-mode)
  :init
  (setq org-appear-trigger 'manual)
  (add-hook 'org-mode-hook 'org-apperance-evil-hack))


(provide 'init-org)
