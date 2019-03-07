;; init-ui.el --- Setup UI.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package all-the-icons
  :defer nil)

(use-package font-lock+
  :quelpa
  (font-lock+ :repo "emacsmirror/font-lock-plus" :fetcher github))

(use-package all-the-icons-ivy
  :hook (after-init . all-the-icons-ivy-setup))

(use-package doom-modeline
  :hook ((after-init . doom-modeline-mode)
         (doom-modeline-mode . setup-custom-doom-modeline))
  :config
  (progn
    (setq
     find-file-visit-truename t  ; display the real names for symlink files
     ;; doom-modeline-height 21
     doom-modeline-lsp nil
     doom-modeline-persp-name nil
     doom-modeline-github nil
     doom-modeline-buffer-file-name-style 'truncate-with-project
     doom-modeline-major-mode-color-icon t)

    (doom-modeline-def-segment my-python-venv
      "The current python virtual environment state."
      (when (eq major-mode 'python-mode)
        (if (eq python-shell-virtualenv-root nil)
            ""
          (propertize
           (let ((base-dir-name (file-name-nondirectory (substring python-shell-virtualenv-root 0 -1))))
             (if (< 10 (length base-dir-name))
                 (format " (%s..)" (substring base-dir-name 0 8))
               (format " (%s)" base-dir-name)))
           'face (if (doom-modeline--active) 'doom-modeline-buffer-major-mode)))))

    (doom-modeline-def-modeline 'my-modeline-layout
      '(bar workspace-number window-number evil-state god-state ryo-modal xah-fly-keys matches buffer-info remote-host buffer-position parrot selection-info)
      '(misc-info persp-name lsp irc mu4e github debug minor-modes input-method buffer-encoding my-python-venv process vcs checker))

    (defun setup-custom-doom-modeline ()
      (doom-modeline-set-modeline 'my-modeline-layout 'default))))

(use-package doom-themes
  :defer nil
  :config
  (progn
    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)
    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config)
    ;; enable custom treemacs themes
    (doom-themes-treemacs-config)))

(load-theme 'doom-dracula t)

(use-package display-line-numbers-mode
  :ensure nil
  :init
  (setq-default display-line-numbers-type 'relative)
  (global-display-line-numbers-mode 1))

;; Highlight current line number
(use-package hlinum
  :defines linum-highlight-in-all-buffersp
  :hook (global-linum-mode . hlinum-activate)
  :init
  (setq linum-highlight-in-all-buffersp t)
  (custom-set-faces
   `(linum-highlight-face
     ((t (:inherit 'default :background ,(face-background 'default) :foreground ,(face-foreground 'default)))))))

(use-package vim-empty-lines-mode
  :ensure t
  :hook ((eshell-mode . (lambda () (vim-empty-lines-mode -1))))
  :init
  (global-vim-empty-lines-mode))

(use-package hide-mode-line
  :hook (((completion-list-mode
           completion-in-region-mode
           lsp-ui-imenu-mode
	   imenu-list-minor-mode
           neotree-mode
           treemacs-mode)
          . hide-mode-line-mode)))


(provide 'init-ui)

;;; init-ui.el ends here
