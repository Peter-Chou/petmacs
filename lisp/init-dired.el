;; init-dired.el --- Setup dired  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Directory operations
(use-package dired
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :preface
  (defun petmacs/dired-goto-parent-directory ()
    "go up a level using same buffer"
    (interactive)
    (find-alternate-file ".."))

  :config
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  (when (eq system-type 'darwin)
    ;; Suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil)

    (when (executable-find "gls")
      ;; Use GNU ls as `gls' from `coreutils' if available.
      (setq insert-directory-program "gls")))

  (when (or (and (eq system-type 'darwin) (executable-find "gls"))
            (and (not (eq system-type 'darwin)) (executable-find "ls")))
    ;; Using `insert-directory-program'
    (setq ls-lisp-use-insert-directory-program t)

    ;; Show directory first
    (setq dired-listing-switches "-alh --group-directories-first"))
  (evil-define-key 'normal dired-mode-map (kbd "RET") 'dired-find-alternate-file) 
  ;; was dired-advertised-find-file
  (evil-define-key 'normal dired-mode-map (kbd "f") 'dired-find-alternate-file) 
  ;; was dired-up-director
  (evil-define-key 'normal dired-mode-map (kbd "^") 'petmacs/dired-goto-parent-directory)  
  ;; kill current buffer when leaving dired mode
  (evil-define-key 'normal dired-mode-map (kbd "q") 'kill-this-buffer)
  :init
  (put 'dired-find-alternate-file 'disabled nil)
  )

;; Quick sort dired buffers via hydra
(use-package dired-quick-sort
  :bind (:map dired-mode-map
              ("S" . hydra-dired-quick-sort/body)))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :init
  (require 'font-lock+))

(use-package diredfl
  :init
  (diredfl-global-mode 1))

(use-package dired-aux :ensure nil)
(use-package dired-x
  :ensure nil
  :demand
  :config
  (let ((cmd (cond
              ((and (display-graphic-p) (eq system-type 'darwin)) "open")
              ((and (display-graphic-p) (eq system-type 'gnu/linux)) "xdg-open")
              ((eq system-type 'windows-nt) "start")
              (t ""))))
    (setq dired-guess-shell-alist-user
          `(("\\.pdf\\'" ,cmd)
            ("\\.docx\\'" ,cmd)
            ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'"
             ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd))))

  (setq dired-omit-files
        (concat dired-omit-files
                "\\|^.DS_Store$\\|^.projectile$\\|^.git$\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*")))

(provide 'init-dired)

;;; init-dired.el ends here