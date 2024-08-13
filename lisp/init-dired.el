;; -*- lexical-binding: t no-byte-compile: t -*-

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Directory operations
(use-package dired
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :preface
  (defun petmacs/dired-goto-parent-directory ()
    "go up a level using same buffer"
    (interactive)
    (find-alternate-file ".."))

  ;; show diectory first
  (defun petmacs//dired-sort ()
    "Sort dired listings with directories first."
    (save-excursion
      (let (buffer-read-only)
	    (forward-line 2) ;; beyond dir. header
	    (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
      (set-buffer-modified-p nil)))
  :init
  (setq dired-kill-when-opening-new-dired-buffer t
        ;; Always delete and copy recursively
        dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-dwim-target t)
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key evil-normal-state-map (kbd "-") 'dired-jump)
  :config
  (when sys/macp
    ;; Suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil)

    (when (executable-find "gls")
      ;; Use GNU ls as `gls' from `coreutils' if available.
      (setq insert-directory-program "gls")))

  (when (or (and sys/macp (executable-find "gls"))
            (and (not sys/win32p) (and (not sys/macp) (executable-find "ls"))))

    ;; Using `insert-directory-program'
    (setq ls-lisp-use-insert-directory-program t)
    ;; Show directory first
    (unless sys/win32p
      (setq dired-listing-switches "-alh --group-directories-first")))

  (when sys/win32p
    (setq dired-listing-switches "-alh")  ;; show human readable file size
    (defadvice dired-readin
	    (after dired-after-updating-hook first () activate)
      "Sort dired listings with directories first before adding marks."
      (petmacs//dired-sort)))

  (evil-define-key 'normal dired-mode-map (kbd "-") 'petmacs/dired-goto-parent-directory))

;; Quick sort dired buffers via hydra
(use-package dired-quick-sort
  :bind (:map dired-mode-map
         ("S" . hydra-dired-quick-sort/body)))

;; Show git info in dired
(use-package dired-git-info
  :bind (:map dired-mode-map
         (")" . dired-git-info-mode)))

;; Allow rsync from dired buffers
(use-package dired-rsync
  :bind (:map dired-mode-map
         ("C-c C-r" . dired-rsync)))

;; Colorful dired
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; Shows icons
(use-package nerd-icons-dired
  :diminish
  :when (icons-displayable-p)
  :custom-face
  (nerd-icons-dired-dir-face ((t (:inherit nerd-icons-dsilver :foreground unspecified))))
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package dired-x
  :ensure nil
  :demand t
  :config
  (let ((cmd (cond (sys/mac-x-p "open")
                   (sys/linux-x-p "xdg-open")
                   (sys/win32p "start")
                   (t ""))))
    (setq dired-guess-shell-alist-user
          `(("\\.pdf\\'" ,cmd)
            ("\\.docx\\'" ,cmd)
            ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd))))

  (setq dired-omit-files
        (concat dired-omit-files
                "\\|^.DS_Store$\\|^.projectile$\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$")))

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired))

;; Extra Dired functionality
(use-package dired-aux :ensure nil)

(provide 'init-dired)
