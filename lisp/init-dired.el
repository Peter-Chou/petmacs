;; -*- lexical-binding: t no-byte-compile: t -*-

(require 'init-const)
(require 'init-custom)

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
    (setq dired-listing-switches "-alh --group-directories-first"))

  (when sys/win32p
    (setq dired-listing-switches "-alh")  ;; show human readable file size
    (defadvice dired-readin
	    (after dired-after-updating-hook first () activate)
      "Sort dired listings with directories first before adding marks."
      (petmacs//dired-sort)))

  (evil-define-key 'normal dired-mode-map (kbd "-") 'petmacs/dired-goto-parent-directory))

;; Allow rsync from dired buffers
(use-package dired-rsync)

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
                "\\|^.DS_Store$\\|^.projectile$\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*")))

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package all-the-icons-dired
  :diminish
  :hook (dired-mode . (lambda ()
                        (when (icons-displayable-p)
                          (all-the-icons-dired-mode))))
  :config
  (with-no-warnings
    (defun my-all-the-icons-dired--refresh ()
      "Display the icons of files in a dired buffer."
      (all-the-icons-dired--remove-all-overlays)
      ;; NOTE: don't display icons it too many items
      (if (<= (count-lines (point-min) (point-max)) 1000)
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when (dired-move-to-filename nil)
                (let ((case-fold-search t))
                  (when-let* ((file (dired-get-filename 'relative 'noerror))
                              (icon (if (file-directory-p file)
                                        (all-the-icons-icon-for-dir
                                         file
                                         :face 'all-the-icons-dired-dir-face
                                         :height 0.9
                                         :v-adjust all-the-icons-dired-v-adjust)
                                      (apply #'all-the-icons-icon-for-file
                                             file
                                             (append
                                              '(:height 0.9)
                                              `(:v-adjust ,all-the-icons-dired-v-adjust)
                                              (when all-the-icons-dired-monochrome
                                                `(:face ,(face-at-point))))))))
                    (if (member file '("." ".."))
                        (all-the-icons-dired--add-overlay (dired-move-to-filename) "   \t")
                      (all-the-icons-dired--add-overlay (dired-move-to-filename) (concat " " icon "\t"))))))
              (forward-line 1)))
        (message "Not display icons because of too many items.")))
    (advice-add #'all-the-icons-dired--refresh :override #'my-all-the-icons-dired--refresh)))

;; Extra Dired functionality
(use-package dired-aux :ensure nil)

(provide 'init-dired)
