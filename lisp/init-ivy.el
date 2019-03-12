;; init-ivy.el --- Setup Ivy.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package counsel
  :diminish ivy-mode counsel-mode
  :defines (projectile-completion-system magit-completing-read-function)
  :bind (("C-s" . swiper)
         ("C-S-s" . swiper-all)

         ("C-c C-r" . ivy-resume)
         ("C-c v p" . ivy-push-view)
         ("C-c v o" . ivy-pop-view)
         ("C-c v ." . ivy-switch-view)

         :map counsel-mode-map
         ([remap swiper] . counsel-grep-or-swiper)
         ("C-x C-r" . counsel-recentf)
         ("C-x j" . counsel-mark-ring)

         ("C-c L" . counsel-load-library)
         ("C-c P" . counsel-package)
         ("C-c f" . counsel-find-library)
         ("C-c g" . counsel-grep)
         ("C-c h" . counsel-command-history)
         ("C-c i" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c l" . counsel-locate)
         ("C-c r" . counsel-rg)
         ("C-c z" . counsel-fzf)

         ("C-c c L" . counsel-load-library)
         ("C-c c P" . counsel-package)
         ("C-c c a" . counsel-apropos)
         ("C-c c e" . counsel-colors-emacs)
         ("C-c c f" . counsel-find-library)
         ("C-c c g" . counsel-grep)
         ("C-c c h" . counsel-command-history)
         ("C-c c i" . counsel-git)
         ("C-c c j" . counsel-git-grep)
         ("C-c c l" . counsel-locate)
         ("C-c c m" . counsel-minibuffer-history)
         ("C-c c o" . counsel-outline)
         ("C-c c p" . counsel-pt)
         ("C-c c r" . counsel-rg)
         ("C-c c s" . counsel-ag)
         ("C-c c t" . counsel-load-theme)
         ("C-c c u" . counsel-unicode-char)
         ("C-c c w" . counsel-colors-web)
         ("C-c c z" . counsel-fzf)

         :map ivy-minibuffer-map
         ("C-w" . ivy-yank-word)

         :map counsel-find-file-map
         ("C-h" . counsel-up-directory)

         :map swiper-map
         ("M-%" . swiper-query-replace))
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :config
  (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers

  (setq ivy-use-selectable-prompt t)
  (setq ivy-use-virtual-buffers t)    ; Enable bookmarks and recentf
  (setq ivy-height 15)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-on-del-error-function nil)

  (defun petmacs//ivy-format-function-arrow (cands)
    "Transform CANDS into a string for minibuffer."
    (ivy--format-function-generic
     (lambda (str)
       (concat (if (char-displayable-p ?▶) "▶ " "> ")
               (ivy--add-face str 'ivy-current-match)))
     (lambda (str)
       (concat "  " str))
     cands
     "\n"))
  (setq ivy-format-function 'petmacs//ivy-format-function-arrow)

  (setq swiper-action-recenter t)
  (setq counsel-find-file-at-point t)
  (setq counsel-yank-pop-separator "\n-------\n")

  ;; Use faster search tools: ripgrep or the silver search
  (let ((cmd (cond ((executable-find "rg")
                    "rg -S --no-heading --line-number --color never '%s' %s")
                   ((executable-find "ag")
                    "ag -S --noheading --nocolor --nofilename --numbers '%s' %s")
                   (t counsel-grep-base-command))))
    (setq counsel-grep-base-command cmd))
  ;; Occur
  (evil-set-initial-state 'ivy-occur-grep-mode 'normal)
  (evil-make-overriding-map ivy-occur-mode-map 'normal)

  ;; Integration with `projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  ;; Integration with `magit'
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read)))

;; Enhance fuzzy matching
(use-package flx)

;; Enhance M-x
(use-package amx)

;; Integrate yasnippet
(use-package ivy-yasnippet
  :bind ("C-c C-y" . ivy-yasnippet)
  :config (advice-add #'ivy-yasnippet--preview :override #'ignore))

;; Ivy integration for Projectile
(use-package counsel-projectile
  :init (counsel-projectile-mode 1))

;; Tramp ivy interface
(use-package counsel-tramp
  :bind (:map counsel-mode-map
              ("C-c c v" . counsel-tramp)))

;; Use ivy as the interface to select from xref candidates
(use-package ivy-xref
  :ensure t
  :init
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
  (setq xref-prompt-for-identifier '(not xref-find-definitions
                                         xref-find-definitions-other-window
                                         xref-find-definitions-other-frame
                                         xref-find-references)))

(use-package ivy-hydra)

(provide 'init-ivy)

;;; init-ivy.el ends here
