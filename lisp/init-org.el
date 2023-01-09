;; -*- lexical-binding: t no-byte-compile: t -*-

(require 'init-custom)

(require'org-tempo) ;; start easy template

(use-package org
  ;; :pin gnu
  :preface
  (defun petmacs/org-mode-setup ()
    (org-indent-mode)
    (variable-pitch-mode 1)
    (visual-line-mode 1))
  (defun petmacs/org-font-setup ()
    ;; Replace list hyphen with dot
    (font-lock-add-keywords 'org-mode
                            '(("^ *\\([-]\\) "
                               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
    ;; Set faces for heading levels
    (dolist (face '((org-level-1 . 1.2)
                    (org-level-2 . 1.1)
                    (org-level-3 . 1.05)
                    (org-level-4 . 1.0)
                    (org-level-5 . 1.1)
                    (org-level-6 . 1.1)
                    (org-level-7 . 1.1)
                    (org-level-8 . 1.1)))
      (set-face-attribute (car face) nil :font petmacs-font :weight 'regular :height (cdr face)))
    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))
  :pin melpa
  :custom-face (org-ellipsis ((t (:foreground unspecified))))
  :hook ((org-mode . petmacs/org-mode-setup)
         ((org-babel-after-execute org-mode) . org-redisplay-inline-images)) ; display image
  :init
  (make-directory (expand-file-name "data/gtd" user-emacs-directory) t)
  :config
  (petmacs/org-font-setup)
  (setq org-modules nil                 ; Faster loading
        org-directory (expand-file-name "data/gtd" user-emacs-directory)

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
                              (prettify-symbols-mode -1)))))

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

  (setq org-projectile-projects-file (expand-file-name "todos.org" (concat user-emacs-directory "data/gtd"))
        org-agenda-files (append org-agenda-files (org-projectile-todo-files))
        org-projectile-per-project-filepath "todos.org")

  (unless (file-exists-p org-projectile-projects-file)
    (write-region "" "" org-projectile-projects-file))

  (with-eval-after-load 'org-capture
    (require 'org-projectile)))

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
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


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
