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
  (setq dired-kill-when-opening-new-dired-buffer t)
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key evil-normal-state-map (kbd "-") 'dired-jump)
  :config
  ;; Always delete and copy recursively
  (setq ;; dired-recursive-deletes 'top  ;; “top” means ask once
   dired-recursive-deletes 'always
   dired-recursive-copies 'always
   dired-dwim-target t)

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

  ;; was dired-advertised-find-file
  ;; (evil-define-key 'normal dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  ;; (evil-define-key 'normal dired-mode-map (kbd "f") 'dired-find-alternate-file)
  ;; was dired-up-director
  ;; (evil-define-key 'normal dired-mode-map (kbd "^") 'petmacs/dired-goto-parent-directory)
  (evil-define-key 'normal dired-mode-map (kbd "-") 'petmacs/dired-goto-parent-directory)
  (evil-define-key 'normal dired-mode-map (kbd "F") 'dired-create-empty-file)
  ;; kill current buffer when leaving dired mode
  ;; (evil-define-key 'normal dired-mode-map (kbd "q") 'kill-this-buffer)
  )

(use-package dired-x
  :ensure nil
  :demand t
  :config
  (let ((cmd (cond
              ((and (display-graphic-p) sys/macp) "open")
              ((and (display-graphic-p) sys/linuxp) "xdg-open")
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
                "\\|^.DS_Store$\\|^.svn$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*")))

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired))

(use-package diredfl
  :init
  (diredfl-global-mode 1))

(use-package all-the-icons-dired
  :diminish
  :hook (dired-mode . (lambda ()
                        (when (icons-displayable-p)
                          (all-the-icons-dired-mode)))))

(use-package ranger
  :diminish
  :commands (ranger deer deer-jump-other-window ranger-override-dired-mode)
  :init
  (setq ranger-deer-show-details t
	    ranger-cleanup-on-disable t
	    ranger-show-hidden t
	    ranger-parent-depth 1
	    ranger-width-parents 0.12
	    ;; ranger-override-dired-mode t  ;; use ranger as default directory handler
	    ranger-ignored-extensions '("mkv" "iso" "mp4")
        ;; set the max files size (in MB) to preview
	    ranger-max-preview-size 5))

(provide 'init-dired)
