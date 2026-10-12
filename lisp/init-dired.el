;; init-dired.el --- Initialize dired configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Directory configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Directory operations
(use-package dired
  :ensure nil
  :defines ls-lisp-use-insert-directory-program
  :bind (:map dired-mode-map
         ("C-c C-p" . wdired-change-to-wdired-mode))
  :config
  (define-key evil-normal-state-map (kbd "-") 'dired-jump)

  ;; Guess a default target directory
  (setq dired-dwim-target t)

  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)

  (if (and (or sys/macp sys/unixp)
           (string= insert-directory-program "ls"))
      ;; Suppress the warning: `ls does not support --dired'.
      (setq dired-use-ls-dired nil
            dired-listing-switches "-alh")
    ;; Show directory first
    (setq dired-listing-switches "-alh --group-directories-first")))

;; Quick sort dired buffers via hydra
(use-package dired-quick-sort
  :after dired
  :custom (dired-quick-sort-suppress-setup-warning t)
  :hook (after-init . dired-quick-sort-setup))

;; Show git info in dired
(use-package dired-git-info
  :after dired
  :bind (:map dired-mode-map
         (")" . dired-git-info-mode)))

;; Allow rsync from dired buffers
(use-package dired-rsync
  :after dired
  :bind (:map dired-mode-map
         ("C-c C-r" . dired-rsync)))

;; Colorful dired
(use-package diredfl
  :hook dired-mode)

;; Shows icons in dired
(use-package nerd-icons-dired
  :functions (nerd-icons-icon-for-dir my/nerd-icons-icon-for-dir)
  :hook dired-mode
  :init
  (defface nerd-icons-dired-dir-face
    '((t nil))
    "Face for the directory icon."
    :group 'nerd-icons-faces)
  (defun my/nerd-icons-icon-for-dir (dir &rest args)
    (apply #'nerd-icons-icon-for-dir
           dir
           :face 'nerd-icons-dired-dir-face
           args))
  (setq nerd-icons-dired-dir-icon-function #'my/nerd-icons-icon-for-dir))

;; Extra Dired functionality
(use-package dired-aux :ensure nil)
(use-package dired-x
  :ensure nil
  :init
  (when-let* ((cmd (cond (sys/macp "open")
                         (sys/linuxp "xdg-open")
                         (sys/win32p "start")
                         (t nil))))
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
  :config
  (setq dired-omit-files
        (concat dired-omit-files
                "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*")))

(provide 'init-dired)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dired.el ends here
