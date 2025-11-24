;; init-org.el --- Initialize Org configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Org configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(require'org-tempo) ;; start easy template

(use-package org
  :ensure nil
  :custom-face (org-ellipsis ((t (:foreground unspecified))))
  :hook (((org-babel-after-execute org-mode) . org-redisplay-inline-images) ;; display image
         (org-indent-mode . (lambda()
                              (diminish 'org-indent-mode)
                              ;; HACK: Prevent text moving around while using brackets
                              ;; @see https://github.com/seagle0128/.emacs.d/issues/88
                              (make-variable-buffer-local 'show-paren-mode)
                              (setq show-paren-mode nil))))
  :config
  (add-to-list 'org-agenda-files (expand-file-name "gtds"  (getenv "HOME")))
  (setq org-modules nil                 ; Faster loading
        org-todo-keywords
        '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")
          (sequence "?(T)" "??(I)" "?(H)" "|" "?(D)" "?(C)"))
        org-todo-keyword-faces '(("HANGUP" . warning)
                                 ("?" . warning))
        org-priority-faces '((?A . error)
                             (?B . warning)
                             (?C . success))

        ;; Agenda styling
        org-agenda-block-separator ?─
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        (concat (format "%s " (nerd-icons-mdicon "nf-md-arrow_left_bold"))
                "now ─────────────────────────────────────────")

        org-tags-column -80
        org-log-done 'time
        org-catch-invisible-edits 'smart
        org-startup-indented t
        org-ellipsis (if (char-displayable-p ?⏷) "\t⏷" nil)
        org-pretty-entities nil
        org-hide-emphasis-markers t))

(use-package org-projectile
  :preface
  (defun org-projectile/goto-project-todos ()
    (interactive)
    (org-projectile-goto-location-for-project (projectile-project-name))
    (revert-buffer t t))
  (defun petmacs/add-todo-files-to-org-agenda-files ()
    (interactive)
    (setq org-agenda-files (delete-dups (append org-agenda-files (org-projectile-todo-files)))))
  :commands (org-projectile-location-for-project
             org-project-capture-todo-files)
  :hook (emacs-startup . petmacs/add-todo-files-to-org-agenda-files)
  :init
  (require 'org-projectile)
  (setq org-project-capture-default-backend
        (make-instance 'org-project-capture-projectile-backend))
  :config
  (with-eval-after-load 'org-capture
    (require 'org-projectile)
    (org-projectile-per-project)))

;; Add md/gfm backends
(add-to-list 'org-export-backends 'md)
(use-package ox-gfm
  :init (add-to-list 'org-export-backends 'gfm))

(use-package org-modern
  :after org
  :diminish
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :init (setq org-modern-star nil
              org-modern-todo-faces '(("HANGUP" :background "yellow" :foreground "black")
                                      ("DOING" :background "chartreuse" :foreground "black")
                                      ("TODO" :background "cyan" :foreground "black"))
              org-modern-priority nil))

;; Paste with org-mode markup and link
(use-package org-rich-yank
  :after org
  :diminish
  :bind (:map org-mode-map
         ("C-M-y" . org-rich-yank)))


(use-package org-superstar
  :after org
  :if (and (display-graphic-p) (char-displayable-p ?◉))
  :hook (org-mode . org-superstar-mode)
  :init (setq org-superstar-headline-bullets-list '("◉""○""◈""◇""⁕")
              org-superstar-special-todo-items t))

(use-package org-fancy-priorities
  :after org
  :diminish
  :hook (org-mode . org-fancy-priorities-mode)
  :init (setq org-fancy-priorities-list
              (if (and (display-graphic-p) (char-displayable-p ?🅐))
                  '("🅐" "🅑" "🅒" "🅓")
                '("HIGH" "MEDIUM" "LOW" "OPTIONAL"))))

(use-package visual-fill-column
  :preface
  (defun petmacs/org-mode-visual-fill ()
    (setq visual-fill-column-width 100
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))
  :hook (org-mode . petmacs/org-mode-visual-fill))

(use-package org-super-agenda)

(use-package org-appear
  :preface
  (defun org-apperance-evil-hack ()
    (add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start nil t)
    (add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop nil t))
  :after org
  :diminish
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoentities t)
  (org-appear-autokeywords t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-inside-latex t)
  (org-appear-manual-linger t)
  (org-appear-delay 0.5)
  :init
  (setq org-appear-trigger 'manual)
  (add-hook 'org-mode-hook 'org-apperance-evil-hack))

;; Babel
(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)

(defconst load-language-alist
  '((emacs-lisp . t)
    (perl       . t)
    (python     . t)
    (ruby       . t)
    (js         . t)
    (css        . t)
    (sass       . t)
    (C          . t)
    (java       . t)
    (shell      . t)
    (plantuml   . t))
  "Alist of org ob languages.")

;; (use-package ob-go
;;   :init (cl-pushnew '(go . t) load-language-alist))

;; (use-package ob-powershell
;;   :init (cl-pushnew '(powershell . t) load-language-alist))

;; (use-package ob-rust
;;   :init (cl-pushnew '(rust . t) load-language-alist))

;; Install: npm install -g @mermaid-js/mermaid-cli
(use-package ob-mermaid
  :init (cl-pushnew '(mermaid . t) load-language-alist))

(use-package ob-restclient
  :after restclient
  :init (cl-pushnew '(restclient . t) load-language-alist))

(org-babel-do-load-languages 'org-babel-load-languages
                             load-language-alist)

(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
