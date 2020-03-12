;; init-dired.el --- Setup dired  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

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
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key evil-normal-state-map (kbd "-") 'dired-jump)
  :config
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'top)  ;; “top” means ask once
  (setq dired-recursive-copies 'always)

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


  (evil-define-key 'normal dired-mode-map
    "RET" 'dired-find-alternate-file
    "f" 'dired-find-alternate-file
    "^" 'petmacs/dired-goto-parent-directory
    "-" 'petmacs/dired-goto-parent-directory
    "q" 'kill-this-buffer
    "j" 'dired-next-line
    "k" 'dired-previous-line
    "." 'dired-clean-directory
    "~" 'dired-flag-backup-files
    "A" 'dired-do-find-regexp
    "C" 'dired-do-copy
    "B" 'dired-do-byte-compile
    "D" 'dired-do-delete
    "gG" 'dired-do-chgrp ;; FIXME: This can probably live on a better binding.
    "H" 'dired-do-hardlink
    "L" 'dired-do-load
    "M" 'dired-do-chmod
    "O" 'dired-do-chown
    "P" 'dired-do-print
    "Q" 'dired-do-find-regexp-and-replace
    "R" 'dired-do-rename
    "S" 'dired-do-symlink
    "T" 'dired-do-touch
    "X" 'dired-do-shell-command
    "Z" 'dired-do-compress
    "c" 'dired-do-compress-to
    "!" 'dired-do-shell-command
    "&" 'dired-do-async-shell-command
    ;; Comparison commands
    "=" 'dired-diff
    ;; Tree Dired commands
    (kbd "M-C-?") 'dired-unmark-all-files
    (kbd "M-C-d") 'dired-tree-down
    (kbd "M-C-u") 'dired-tree-up
    (kbd "M-C-n") 'dired-next-subdir
    (kbd "M-C-p") 'dired-prev-subdir
    ;; move to marked files
    (kbd "M-{") 'dired-prev-marked-file
    (kbd "M-}") 'dired-next-marked-file
    ;; mark
    "*" nil
    "**" 'dired-mark-executables
    "*/" 'dired-mark-directories
    "*@" 'dired-mark-symlinks
    "*%" 'dired-mark-files-regexp
    "*(" 'dired-mark-sexp
    "*." 'dired-mark-extension
    "*O" 'dired-mark-omitted
    "*c" 'dired-change-marks
    "*s" 'dired-mark-subdir-files
    "*m" 'dired-mark
    "*u" 'dired-unmark
    "*?" 'dired-unmark-all-files
    "*!" 'dired-unmark-all-marks
    "U" 'dired-unmark-all-marks
    (kbd "* <delete>") 'dired-unmark-backward
    (kbd "* C-n") 'dired-next-marked-file
    (kbd "* C-p") 'dired-prev-marked-file
    "*t" 'dired-toggle-marks
    ;; Lower keys for commands not operating on all the marked files
    "a" 'dired-find-alternate-file
    "d" 'dired-flag-file-deletion
    "gf" 'dired-find-file
    (kbd "C-m") 'dired-find-file
    "gr" 'revert-buffer
    "i" 'dired-toggle-read-only
    "I" 'dired-maybe-insert-subdir
    "J" 'dired-goto-file
    "K" 'dired-do-kill-lines
    "r" 'dired-do-redisplay
    "m" 'dired-mark
    "t" 'dired-toggle-marks
    "u" 'dired-unmark                   ; also "*u"
    "W" 'browse-url-of-dired-file
    "x" 'dired-do-flagged-delete
    "gy" 'dired-show-file-type ;; FIXME: This could probably go on a better key.
    "Y" 'dired-copy-filename-as-kill
    "+" 'dired-create-directory
    ;; open
    (kbd "S-<return>") 'dired-find-file-other-window
    (kbd "M-RET") 'dired-display-file
    "gO" 'dired-find-file-other-window
    "go" 'dired-view-file
    ;; sort
    "o" 'dired-sort-toggle-or-edit
    ;; moving
    "gj" 'dired-next-dirline
    "gk" 'dired-prev-dirline
    "[[" 'dired-prev-dirline
    "]]" 'dired-next-dirline
    "<" 'dired-prev-dirline
    ">" 'dired-next-dirline
    "^" 'dired-up-directory
    ;; "-" 'dired-up-directory
    " " 'dired-next-line
    [?\S-\ ] 'dired-previous-line
    [remap next-line] 'dired-next-line
    [remap previous-line] 'dired-previous-line
    ;; hiding
    "g$" 'dired-hide-subdir ;; FIXME: This can probably live on a better binding.
    (kbd "M-$") 'dired-hide-all
    "(" 'dired-hide-details-mode
    ;; isearch
    (kbd "M-s a C-s")   'dired-do-isearch
    (kbd "M-s a M-C-s") 'dired-do-isearch-regexp
    (kbd "M-s f C-s")   'dired-isearch-filenames
    (kbd "M-s f M-C-s") 'dired-isearch-filenames-regexp
    ;; misc
    [remap read-only-mode] 'dired-toggle-read-only
    ;; `toggle-read-only' is an obsolete alias for `read-only-mode'
    [remap toggle-read-only] 'dired-toggle-read-only
    "g?" 'dired-summary
    (kbd "<delete>") 'dired-unmark-backward
    [remap undo] 'dired-undo
    [remap advertised-undo] 'dired-undo
    ;; thumbnail manipulation (image-dired)
    (kbd "C-t d") 'image-dired-display-thumbs
    (kbd "C-t t") 'image-dired-tag-files
    (kbd "C-t r") 'image-dired-delete-tag
    (kbd "C-t j") 'image-dired-jump-thumbnail-buffer
    (kbd "C-t i") 'image-dired-dired-display-image
    (kbd "C-t x") 'image-dired-dired-display-external
    (kbd "C-t a") 'image-dired-display-thumbs-append
    (kbd "C-t .") 'image-dired-display-thumb
    (kbd "C-t c") 'image-dired-dired-comment-files
    (kbd "C-t f") 'image-dired-mark-tagged-files
    (kbd "C-t C-t") 'image-dired-dired-toggle-marked-thumbs
    (kbd "C-t e") 'image-dired-dired-edit-comment-and-tags
    ;; encryption and decryption (epa-dired)
    ";d" 'epa-dired-do-decrypt
    ";v" 'epa-dired-do-verify
    ";s" 'epa-dired-do-sign
    ";e" 'epa-dired-do-encrypt
    ) 
  ;; was dired-advertised-find-file
  ;; (evil-define-key 'normal dired-mode-map (kbd "f") 'dired-find-alternate-file) 
  ;; was dired-up-director
  ;; (evil-define-key 'normal dired-mode-map (kbd "^") 'petmacs/dired-goto-parent-directory)  
  ;; (evil-define-key 'normal dired-mode-map (kbd "-") 'petmacs/dired-goto-parent-directory)  
  ;; kill current buffer when leaving dired mode
  ;; (evil-define-key 'normal dired-mode-map (kbd "q") 'kill-this-buffer)
  )

;; Quick sort dired buffers via hydra
(use-package dired-quick-sort
  :bind (:map dired-mode-map
              ("S" . hydra-dired-quick-sort/body)))

  ;; Shows icons
  (use-package all-the-icons-dired
    :diminish
    :if (icons-displayable-p)
    :hook (dired-mode . all-the-icons-dired-mode)
    :config
    (with-no-warnings
      (defun my-all-the-icons-dired--refresh ()
        "Display the icons of files in a dired buffer."
        (all-the-icons-dired--remove-all-overlays)
        ;; NOTE: don't display icons it too many items
        (if (<= (count-lines (point-min) (point-max)) 1000)
            (save-excursion
              ;; TRICK: Use TAB to align icons
              (setq-local tab-width 1)

              (goto-char (point-min))
              (while (not (eobp))
                (let ((file (dired-get-filename 'verbatim t)))
                  (when file
                    (let ((icon (if (file-directory-p file)
                                    (all-the-icons-icon-for-dir file nil "")
                                  (all-the-icons-icon-for-file file :v-adjust all-the-icons-dired-v-adjust))))
                      (if (member file '("." ".."))
                          (all-the-icons-dired--add-overlay (point) " \t")
                        (all-the-icons-dired--add-overlay (point) (concat icon "\t"))))))
                (dired-next-line 1)))
          (message "Not display icons because of too many items.")))
      (advice-add #'all-the-icons-dired--refresh
                  :override #'my-all-the-icons-dired--refresh)))

;; Allow rsync from dired buffers
(use-package dired-rsync
  :bind (:map dired-mode-map
                ("C-c C-r" . dired-rsync)))

(use-package diredfl
  :init
  (diredfl-global-mode 1))

(use-package dired-aux :ensure nil)
(use-package dired-x
  :ensure nil
  :demand
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
                "\\|^.DS_Store$\\|^.projectile$\\|^.git$\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*")))

;; (use-package ranger
;;   :diminish
;;   :commands (ranger deer deer-jump-other-window ranger-override-dired-mode)
;;   :init
;;   (setq ranger-override-dired t)
;;   (setq ranger-deer-show-details t)
;;   (setq ranger-cleanup-on-disable t)
;;   (setq ranger-show-hidden t)
;;   (setq ranger-parent-depth 1)
;;   (setq ranger-width-parents 0.12)
;;   (ranger-override-dired-mode t)  ;; use ranger as default directory handler
;;   (setq ranger-ignored-extensions '("mkv" "iso" "mp4"))
;;   ;; set the max files size (in MB) to preview
;;   (setq ranger-max-preview-size 5)
;;   ;; allow '-' to enter ranger
;;   (define-key evil-normal-state-map (kbd "-") 'deer)
;;   :config
;;   (define-key ranger-mode-map (kbd "-") 'ranger-up-directory)
;;   (with-eval-after-load 'counsel-projectile
;;     ;; open deer when switch into one project
;;     (counsel-projectile-modify-action
;;      'counsel-projectile-switch-project-action
;;      '((add ("." deer "open ‘deer’ at the root of the project") 1)))))

(provide 'init-dired)

;;; init-dired.el ends here
