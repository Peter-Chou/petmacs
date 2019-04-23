;; init-dired.el --- Setup dired  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-variable))

;; Directory operations
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode))
  :config
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  (when sys/macp
    ;; Suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil)

    (when (executable-find "gls")
      ;; Use GNU ls as `gls' from `coreutils' if available.
      (setq insert-directory-program "gls")))

  (when (or (and sys/macp (executable-find "gls"))
            (and (not sys/macp) (executable-find "ls")))
    ;; Using `insert-directory-program'
    (setq ls-lisp-use-insert-directory-program t)

    ;; Show directory first
    (setq dired-listing-switches "-alh --group-directories-first")

    ;; Quick sort dired buffers via hydra
    (use-package dired-quick-sort
      :bind (:map dired-mode-map
                  ("S" . hydra-dired-quick-sort/body))))

  ;; Colourful dired
  (use-package diredfl
    :init (diredfl-global-mode 1))

  ;; Shows icons
  (use-package all-the-icons-dired
    :diminish
    :custom-face (all-the-icons-dired-dir-face ((t (:foreground nil))))
    :hook (dired-mode . all-the-icons-dired-mode)
    :config
    (defun my-all-the-icons-dired--display ()
      "Display the icons of files without colors in a dired buffer."

      ;; Fix: not display icons after dired commands (e.g insert-subdir, create-directory)
      ;; @see https://github.com/jtbm37/all-the-icons-dired/issues/11
      (all-the-icons-dired--reset)

      (when (and (not all-the-icons-dired-displayed) dired-subdir-alist)
        (setq-local all-the-icons-dired-displayed t)
        (let ((inhibit-read-only t)
              (remote-p (and (fboundp 'tramp-tramp-file-p)
                             (tramp-tramp-file-p default-directory))))
          (save-excursion
            (setq-local tab-width 1)
            (goto-char (point-min))
            (while (not (eobp))
              (when (dired-move-to-filename nil)
                (insert "\t")
                (let ((file (dired-get-filename 'verbatim t)))
                  (unless (member file '("." ".."))
                    (let ((filename (dired-get-filename nil t)))
                      (if (file-directory-p filename)
                          (let ((icon
                                 (cond
                                  (remote-p
                                   (all-the-icons-octicon "file-directory" :height 1.0 :face 'all-the-icons-dired-dir-face :v-adjust all-the-icons-dired-v-adjust))
                                  ((file-symlink-p filename)
                                   (all-the-icons-octicon "file-symlink-directory" :height 1.0 :face 'all-the-icons-dired-dir-face :v-adjust all-the-icons-dired-v-adjust))
                                  ((all-the-icons-dir-is-submodule filename)
                                   (all-the-icons-octicon "file-submodule" :height 1.0 :face 'all-the-icons-dired-dir-face :v-adjust all-the-icons-dired-v-adjust))
                                  ((file-exists-p (format "%s/.git" filename))
                                   (all-the-icons-octicon "repo" :height 1.1 :face 'all-the-icons-dired-dir-face :v-adjust all-the-icons-dired-v-adjust ))
                                  (t (let ((matcher (all-the-icons-match-to-alist file all-the-icons-dir-icon-alist)))
                                       (apply (car matcher) (list (cadr matcher) :face 'all-the-icons-dired-dir-face :v-adjust all-the-icons-dired-v-adjust)))))))
                            (insert icon))
                        (insert (all-the-icons-icon-for-file file :v-adjust -0.05))))
                    (insert "\t"))))
              (forward-line 1))))))
    (advice-add #'all-the-icons-dired--display :override #'my-all-the-icons-dired--display))

  ;; Extra Dired functionality
  (use-package dired-aux :ensure nil)
  (use-package dired-x
  :ensure nil
  :demand
  :config
  (let ((cmd (cond
              (sys/mac-x-p "open")
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
                "\\|^.DS_Store$\\|^.projectile$\\|^.git$\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*"))))

(use-package ranger
  :commands (ranger deer deer-jump-other-window ranger-override-dired-mode)
  :init
  (setq ranger-cleanup-on-disable t)
  (setq ranger-show-hidden t)
  (setq ranger-parent-depth 1)
  (setq ranger-width-parents 0.12)
  (setq ranger-ignored-extensions '("mkv" "iso" "mp4"))
  ;; set the max files size (in MB) to preview
  (setq ranger-max-preview-size 5)
  ;; allow '-' to enter ranger
  (define-key evil-normal-state-map (kbd "-") 'deer)
  ;; dired mapping in evil mode
  (evil-define-key 'normal dired-mode-map (kbd "RET") 'dired-find-alternate-file) 
  ;; was dired-advertised-find-file
  (evil-define-key 'normal dired-mode-map (kbd "f") 'dired-find-alternate-file) 
  ;; was dired-up-director
  (evil-define-key 'normal dired-mode-map (kbd "^") 'petmacs/dired-goto-parent-directory)  
  ;; kill current buffer when leaving dired mode
  (evil-define-key 'normal dired-mode-map (kbd "q") 'kill-this-buffer)
  :config
  (define-key ranger-mode-map (kbd "-") 'ranger-up-directory))

(provide 'init-dired)

;;; init-dired.el ends here

