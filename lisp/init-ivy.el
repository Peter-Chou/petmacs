;; init-ivy.el --- Setup Ivy.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package counsel
  ;; C-M-j to create file/dir when name is match part of the exist file/dir
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

  ;; Pre-fill for commands
  ;; @see https://www.reddit.com/r/emacs/comments/b7g1px/withemacs_execute_commands_like_marty_mcfly/
  (defvar my-ivy-fly-commands
    '(query-replace-regexp
      flush-lines
      keep-lines))

  (defun my-ivy-fly-back-to-present ()
    (remove-hook 'pre-command-hook 'my-ivy-fly-back-to-present t)
    (cond ((and (memq last-command my-ivy-fly-commands)
                (equal (this-command-keys-vector) (kbd "M-p")))
           ;; repeat one time to get straight to the first history item
           (setq unread-command-events
                 (append unread-command-events
                         (listify-key-sequence (kbd "M-p")))))
          ((or (memq this-command '(self-insert-command))
               (memq this-command '(ivy-yank-word)))
           (delete-region (point)
                          (point-max)))))

  (defun my-ivy-fly-time-travel ()
    (when (memq this-command my-ivy-fly-commands)
      (let* ((kbd (kbd "M-n"))
             (cmd (key-binding kbd))
             (future (and cmd
                          (with-temp-buffer
                            (when (ignore-errors
                                    (call-interactively cmd) t)
                              (buffer-string))))))
        (when future
          (save-excursion
            (insert (propertize future 'face 'shadow)))
          (add-hook 'pre-command-hook 'my-ivy-fly-back-to-present nil t)))))

  (add-hook 'minibuffer-setup-hook #'my-ivy-fly-time-travel)

  (push (cons 'swiper 'my-fly-swiper) ivy-hooks-alist)
  (defun my-fly-swiper ()
    (let ((sym (with-ivy-window (ivy-thing-at-point))))
      (when sym
        (add-hook 'pre-command-hook 'my-ivy-fly-back-to-present nil t)
        (save-excursion
          (insert (propertize sym 'face 'shadow))))))

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
  :init
  (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point))
  (counsel-projectile-mode 1))

;; Tramp ivy interface
(use-package counsel-tramp
  :bind (:map counsel-mode-map
              ("C-c c v" . counsel-tramp))
  :init
  (setq tramp-default-method "ssh")
  (add-hook 'counsel-tramp-pre-command-hook '(lambda () (global-aggressive-indent-mode 0)
					       (projectile-mode 0)
					       (editorconfig-mode 0)))
  (add-hook 'counsel-tramp-quit-hook '(lambda () (global-aggressive-indent-mode 1)
					(projectile-mode 1)
					(editorconfig-mode 1))))

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
