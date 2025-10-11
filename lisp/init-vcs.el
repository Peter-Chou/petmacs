;; init-vcs.el --- Initialize version control system configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Version control systems.
;;

;;; Code:

(use-package sqlite3
  :demand t)

(use-package magit
  ;; :hook (magit-mode . magit-wip-mode)
  :init
  (setq magit-diff-refine-hunk t
        git-commit-major-mode 'git-commit-elisp-text-mode
        magit-format-file-function #'magit-format-file-nerd-icons
        magit-process-finish-apply-ansi-colors t
        magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  :config
  (when sys/win32p
    (setenv "GIT_ASKPASS" "git-gui--askpass"))

  ;; Exterminate Magit buffers
  (with-no-warnings
    (defun my-magit-kill-buffers (&rest _)
      "Restore window configuration and kill all Magit buffers."
      (interactive)
      (magit-restore-window-configuration)
      (let ((buffers (magit-mode-get-buffers)))
        (when (eq major-mode 'magit-status-mode)
          (mapc (lambda (buf)
                  (with-current-buffer buf
                    (if (and magit-this-process
                             (eq (process-status magit-this-process) 'run))
                        (bury-buffer buf)
                      (kill-buffer buf))))
                buffers))))
    (setq magit-bury-buffer-function #'my-magit-kill-buffers))

  ;; When 'C-c C-c' is pressed in the magit commit message buffer,
  ;; delete the magit-diff buffer related to the current repo.
  (defun kill-magit-diff-buffer-in-current-repo (&rest _)
    "Delete the magit-diff buffer related to the current repo"
    (let ((magit-diff-buffer-in-current-repo
           (magit-mode-get-buffer 'magit-diff-mode)))
      (kill-buffer magit-diff-buffer-in-current-repo)))

  (add-hook 'git-commit-setup-hook
            (lambda ()
              (add-hook 'with-editor-post-finish-hook
  			            #'kill-magit-diff-buffer-in-current-repo
  			            nil t))) ; the t is important

  ;; ;; kill magit status buffer when quitting magit status
  (define-key magit-mode-map (kbd "q") (lambda()
  					                     (interactive)
  					                     (magit-mode-bury-buffer t)))

  (define-key magit-mode-map (kbd "M-1") 'winum-select-window-1)
  (define-key magit-mode-map (kbd "M-2") 'winum-select-window-2)
  (define-key magit-mode-map (kbd "M-3") 'winum-select-window-3)
  (define-key magit-mode-map (kbd "M-4") 'winum-select-window-4)
  (define-key magit-mode-map (kbd "M-5") 'winum-select-window-5)
  (define-key magit-mode-map (kbd "M-6") 'winum-select-window-6)
  (define-key magit-mode-map (kbd "M-7") 'winum-select-window-7)
  (define-key magit-mode-map (kbd "M-8") 'winum-select-window-8))

;; Walk through git revisions of a file
(use-package git-timemachine
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit success :foreground unspecified))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning :foreground unspecified))))
  :bind (:map vc-prefix-map
         ("t" . git-timemachine))
  :hook ((git-timemachine-mode . (lambda ()
                                   "Improve `git-timemachine' buffers."
                                   ;; Display different colors in mode-line
                                   (if (facep 'mode-line-active)
                                       (face-remap-add-relative 'mode-line-active 'custom-state)
                                     (face-remap-add-relative 'mode-line 'custom-state))

                                   ;; Highlight symbols in elisp
                                   (and (derived-mode-p 'emacs-lisp-mode)
                                        (fboundp 'highlight-defined-mode)
                                        (highlight-defined-mode t))

                                   ;; Display line numbers
                                   (and (derived-mode-p 'prog-mode 'yaml-mode)
                                        (fboundp 'display-line-numbers-mode)
                                        (display-line-numbers-mode t))))
         (before-revert . (lambda ()
                            (when (bound-and-true-p git-timemachine-mode)
                              (user-error "Cannot revert the timemachine buffer"))))))

;; Pop up last commit information of current line
(use-package git-messenger
  :bind (:map vc-prefix-map
         ("p" . git-messenger:popup-message)
         :map git-messenger-map
         ("m" . git-messenger:copy-message))
  :init (setq git-messenger:show-detail t
              git-messenger:use-magit-popup t)
  :config
  (with-no-warnings
    (with-eval-after-load 'hydra
      (defhydra git-messenger-hydra (:color blue)
        ("s" git-messenger:popup-show "show")
        ("c" git-messenger:copy-commit-id "copy hash")
        ("m" git-messenger:copy-message "copy message")
        ("," (catch 'git-messenger-loop (git-messenger:show-parent)) "go parent")
        ("q" git-messenger:popup-close "quit")))

    (defun my-git-messenger:format-detail (vcs commit-id author message)
      (if (eq vcs 'git)
          (let ((date (git-messenger:commit-date commit-id))
                (colon (propertize ":" 'face 'font-lock-comment-face)))
            (concat
             (format "%s%s %s \n%s%s %s\n%s  %s %s \n"
                     (propertize "Commit" 'face 'font-lock-keyword-face) colon
                     (propertize (substring commit-id 0 8) 'face 'font-lock-comment-face)
                     (propertize "Author" 'face 'font-lock-keyword-face) colon
                     (propertize author 'face 'font-lock-string-face)
                     (propertize "Date" 'face 'font-lock-keyword-face) colon
                     (propertize date 'face 'font-lock-string-face))
             (propertize (make-string 38 ?─) 'face 'font-lock-comment-face)
             message
             (propertize "\nPress q to quit" 'face '(:inherit (font-lock-comment-face italic)))))
        (git-messenger:format-detail vcs commit-id author message)))

    (defun my-git-messenger:popup-message ()
      "Popup message with `posframe', `pos-tip', `lv' or `message', and dispatch actions with `hydra'."
      (interactive)
      (let* ((hydra-hint-display-type 'message)
             (vcs (git-messenger:find-vcs))
             (file (buffer-file-name (buffer-base-buffer)))
             (line (line-number-at-pos))
             (commit-info (git-messenger:commit-info-at-line vcs file line))
             (commit-id (car commit-info))
             (author (cdr commit-info))
             (msg (git-messenger:commit-message vcs commit-id))
             (popuped-message (if (git-messenger:show-detail-p commit-id)
                                  (my-git-messenger:format-detail vcs commit-id author msg)
                                (cl-case vcs
                                  (git msg)
                                  (svn (if (string= commit-id "-")
                                           msg
                                         (git-messenger:svn-message msg)))
                                  (hg msg)))))
        (setq git-messenger:vcs vcs
              git-messenger:last-message msg
              git-messenger:last-commit-id commit-id)
        (run-hook-with-args 'git-messenger:before-popup-hook popuped-message)
        (git-messenger-hydra/body)
        (cond ((and (fboundp 'posframe-workable-p) (posframe-workable-p))
               (let ((buffer-name "*git-messenger*"))
                 (posframe-show buffer-name
                                :string (concat (propertize "\n" 'face '(:height 0.3))
                                                popuped-message
                                                "\n"
                                                (propertize "\n" 'face '(:height 0.3)))
                                :left-fringe 8
                                :right-fringe 8
                                :max-width (round (* (frame-width) 0.62))
                                :max-height (round (* (frame-height) 0.62))
                                :internal-border-width 1
                                :internal-border-color (face-background 'posframe-border nil t)
                                :background-color (face-background 'tooltip nil t))
                 (unwind-protect
                     (push (read-event) unread-command-events)
                   (posframe-hide buffer-name))))
              ((and (fboundp 'pos-tip-show) (display-graphic-p))
               (pos-tip-show popuped-message))
              ((fboundp 'lv-message)
               (lv-message popuped-message)
               (unwind-protect
                   (push (read-event) unread-command-events)
                 (lv-delete-window)))
              (t (message "%s" popuped-message)))
        (run-hook-with-args 'git-messenger:after-popup-hook popuped-message)))
    (advice-add #'git-messenger:popup-close :override #'ignore)
    (advice-add #'git-messenger:popup-message :override #'my-git-messenger:popup-message)))

;; Resolve diff3 conflicts
(use-package smerge-mode
  :ensure nil
  :diminish
  :pretty-hydra
  ((:title (pretty-hydra-title "Smerge" 'octicon "nf-oct-diff")
    :color pink :quit-key ("q" "C-g"))
   ("Move"
    (("n" smerge-next "next")
     ("p" smerge-prev "previous"))
    "Keep"
    (("b" smerge-keep-base "base")
     ("u" smerge-keep-upper "upper")
     ("l" smerge-keep-lower "lower")
     ("a" smerge-keep-all "all")
     ("RET" smerge-keep-current "current")
     ("C-m" smerge-keep-current "current"))
    "Diff"
    (("<" smerge-diff-base-upper "upper/base")
     ("=" smerge-diff-upper-lower "upper/lower")
     (">" smerge-diff-base-lower "upper/lower")
     ("R" smerge-refine "refine")
     ("E" smerge-ediff "ediff"))
    "Other"
    (("C" smerge-combine-with-next "combine")
     ("r" smerge-resolve "resolve")
     ("k" smerge-kill-current "kill")
     ("ZZ" (lambda ()
             (interactive)
             (save-buffer)
             (bury-buffer))
      "Save and bury buffer" :exit t))))
  :bind (:map smerge-mode-map
         ("C-c m" . smerge-mode-hydra/body))
  :hook ((find-file . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)))))
         (magit-diff-visit-file . (lambda ()
                                    (when smerge-mode
                                      (smerge-mode-hydra/body))))))

;; Open github/gitlab/bitbucket page
(use-package browse-at-remote
  :bind (:map vc-prefix-map
         ("B" . browse-at-remote)))

;; Git related modes
(use-package git-modes)

(use-package git-gutter
  :custom-face
  (git-gutter:modified ((t (:inherit nerd-icons-lsilver :height 0.625 :background nil))))
  (git-gutter:added ((t (:inherit nerd-icons-green :height 0.625 :background nil)))) ;; :weight bold
  (git-gutter:deleted ((t (:inherit nerd-icons-red :height 0.625 :background nil))))
  :hook (after-init . global-git-gutter-mode)
  :init (setq git-gutter:update-interval 1
              ;; git-gutter:window-width 2
              git-gutter:modified-sign (nerd-icons-octicon "nf-oct-diff_modified")
              git-gutter:added-sign (nerd-icons-octicon "nf-oct-diff_added")
              git-gutter:deleted-sign (nerd-icons-octicon "nf-oct-diff_removed")))

;; Show TODOs in magit
(use-package magit-todos
  :after magit-status
  :commands magit-todos-mode
  :init
  (setq magit-todos-nice (if (executable-find "nice") t nil))
  (magit-todos-mode 1))

(provide 'init-vcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-vcs.el ends here
