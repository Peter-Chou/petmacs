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
	org-use-sub-superscripts nil	;; disable ^ _ for (super/sub)script in display
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)"))
        org-todo-keyword-faces '(("HANGUP" . warning))
        org-pretty-entities t
	org-hide-emphasis-markers t
        org-log-done t
        org-startup-with-inline-images t
        org-image-actual-width nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        ;; this is consistent with the value of
        ;; `helm-org-headings-max-depth'.
        org-imenu-depth 8)

  (add-to-list 'org-export-backends 'md)

  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/TODOs.org" "Todo soon")
           "* TODO %? \n  %^t")
          ))

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defvar load-language-list '((emacs-lisp . t)
                               (perl . t)
                               (python . t)
			       (dot . t)
			       (latex . t)
                               (ruby . t)
                               (js . t)
                               (css . t)
                               (sass . t)
                               (C . t)
                               (java . t)
                               (plantuml . t)))

  ;; ob-sh renamed to ob-shell since 26.1.
  (if emacs/>=26p
      (cl-pushnew '(shell . t) load-language-list)
    (cl-pushnew '(sh . t) load-language-list))

  (use-package ob-ipython
    :if (executable-find "jupyter")     ; DO NOT remove
    :init (cl-pushnew '(ipython . t) load-language-list))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list))

(use-package org-agenda
  :ensure nil
  :init
  (setq org-agenda-restore-windows-after-quit t)
  (setq org-agenda-current-time-string "← now")
  (setq org-agenda-time-grid ;; Format is changed from 9.1
        '((daily today require-timed)
          (0900 01000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300 2400)
          "-"
	  "────────────────")))

;; Pomodoro
(use-package org-pomodoro
  :after org-agenda
  :bind (:map org-agenda-mode-map
              ("P" . org-pomodoro)))

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
    (org-projectile-goto-location-for-project (projectile-project-name))
    (revert-buffer))

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

(use-package evil-org
  :preface
  (defun petmacs//evil-org-mode ()
    (evil-org-mode)
    (evil-normalize-keymaps)
    (evil-org-set-key-theme))
  :hook (org-mode . petmacs//evil-org-mode)
  :init
  (setq evil-org-use-additional-insert t
        evil-org-key-theme `(textobjects
                             navigation
                             additional
			     calendar
                             todo))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Preview
(use-package org-preview-html
  :diminish org-preview-html-mode)

;; Visually summarize progress
(use-package org-dashboard)

(use-package org-plus-contrib)

(provide 'init-org)

;;; init-org.el ends here
