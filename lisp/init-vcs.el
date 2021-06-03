;; init-vcs.el --- Setup version control.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package magit
  :pin melpa-stable
  :mode (("\\COMMIT_EDITMSG\\'" . text-mode)
         ("\\MERGE_MSG\\'" . text-mode))
  :bind (("C-x g" . magit-status)
          ("C-x M-g" . magit-dispatch-popup)
          ("C-c M-g" . magit-file-popup))
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  ;; (add-to-list 'magit-log-arguments "--color")

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
  (git-timemachine-minibuffer-author-face ((t (:inherit success))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
  :bind (:map vc-prefix-map
         ("t" . git-timemachine))
  :hook (before-revert . (lambda ()
                           (when (bound-and-true-p git-timemachine-mode)
                             (user-error "Cannot revert the timemachine buffer")))))

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
                                :internal-border-width 1
                                :internal-border-color (face-foreground 'font-lock-comment-face nil t)
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
  :preface
  (with-eval-after-load 'hydra
    (defhydra smerge-hydra
      (:color pink :hint nil :post (smerge-auto-leave))
      "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
      ("n" smerge-next)
      ("p" smerge-prev)
      ("b" smerge-keep-base)
      ("u" smerge-keep-upper)
      ("l" smerge-keep-lower)
      ("a" smerge-keep-all)
      ("RET" smerge-keep-current)
      ("\C-m" smerge-keep-current)
      ("<" smerge-diff-base-upper)
      ("=" smerge-diff-upper-lower)
      (">" smerge-diff-base-lower)
      ("R" smerge-refine)
      ("E" smerge-ediff)
      ("C" smerge-combine-with-next)
      ("r" smerge-resolve)
      ("k" smerge-kill-current)
      ("ZZ" (lambda ()
	      (interactive)
	      (save-buffer)
	      (bury-buffer))
       "Save and bury buffer" :color blue)
      ("q" nil "cancel" :color blue)))
  :hook ((find-file . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)))))
         (magit-diff-visit-file . (lambda ()
                                    (when smerge-mode
				      (smerge-hydra/body))))))

;; Open github/gitlab/bitbucket page
(use-package browse-at-remote
  :bind (:map vc-prefix-map
	 ("B" . browse-at-remote)))

;; (use-package git-gutter
;;   :hook (prog-mode . git-gutter-mode)
;;   :custom
;;   (git-gutter:modified-sign " ")
;;   (git-gutter:added-sign    "+")
;;   (git-gutter:deleted-sign  "-")
;;   :custom-face
;;   (git-gutter:modified ((t (:background "#f1fa8c"))))
;;   (git-gutter:added    ((t (:background "#50fa7b"))))
;;   (git-gutter:deleted  ((t (:background "#ff79c6"))))
;;   :init
;;   (run-with-idle-timer 1 nil 'global-git-gutter-mode)
;;   (setq git-gutter:update-interval 2
;;         ;; git-gutter:modified-sign " "
;;         ;; git-gutter:added-sign "+"
;;         ;; git-gutter:deleted-sign "-"
;;         ;; git-gutter:diff-option "-w"
;;         git-gutter:ask-p nil
;;         git-gutter:verbosity 0
;;         git-gutter:handled-backends '(git hg bzr svn)
;;         git-gutter:hide-gutter t))


;; Show tasks
(use-package pcre2el) ;; magit-todos dependency

;; Show TODOs in magit
(when emacs/>=25.2p
  (use-package magit-todos
    :init
    (setq magit-todos-nice (if (executable-find "nice") t nil))
    (setq magit-todos-exclude-globs '("third_party"))
    (setq magit-todos-section-map
	  (let ((map (make-sparse-keymap)))
	    (define-key map "j" #'evil-next-visual-line)
	    (define-key map "k" #'evil-previous-visual-line)
	    map))
    (magit-todos-mode 1)))

;; Display transient in child frame
(when (childframe-workable-p)
  (use-package transient-posframe
    :diminish
    :custom-face
    (transient-posframe ((t (:inherit tooltip))))
    (transient-posframe-border ((t (:background ,(face-foreground 'font-lock-comment-face nil t)))))
    :hook (after-init . transient-posframe-mode)
    :init
    (setq transient-posframe-border-width 3
          transient-posframe-min-height 22
          transient-posframe-min-width nil
          transient-posframe-parameters '((left-fringe . 8)
                                          (right-fringe . 8)))
    :config
    (add-hook 'after-load-theme-hook
              (lambda ()
                (custom-set-faces
                 '(transient-posframe ((t (:inherit tooltip))))
                 `(transient-posframe-border ((t (:background ,(face-foreground 'font-lock-comment-face nil t))))))))

    (with-no-warnings
      (defun my-transient-posframe--show-buffer (buffer _alist)
        "Show BUFFER in posframe and we do not use _ALIST at this period."
        (when (posframe-workable-p)
          (let ((posframe (posframe-show
                           buffer
			   :font transient-posframe-font
			   :position (point)
			   :poshandler transient-posframe-poshandler
			   :background-color (face-attribute 'transient-posframe :background nil t)
			   :foreground-color (face-attribute 'transient-posframe :foreground nil t)
			   :min-width (or transient-posframe-min-width (round (* (frame-width) 0.62)))
			   :min-height transient-posframe-min-height
                           :lines-truncate t
			   :internal-border-width transient-posframe-border-width
			   :internal-border-color (face-attribute 'transient-posframe-border :background nil t)
			   :override-parameters transient-posframe-parameters)))
            (frame-selected-window posframe))))
      (advice-add #'transient-posframe--show-buffer :override #'my-transient-posframe--show-buffer)

      (defun my-transient-posframe--render-buffer ()
        (with-current-buffer (get-buffer-create transient--buffer-name)
          (goto-char (point-min))
          (insert (propertize "\n" 'face '(:height 0.3)))
          (goto-char (point-max))
          (insert (propertize "\n\n" 'face '(:height 0.3)))))
      (advice-add #'transient--show :after #'my-transient-posframe--render-buffer))))

;; Git related modes
(use-package gitattributes-mode)
(use-package gitignore-mode)
(use-package gitconfig-mode)
(use-package git-commit)

(provide 'init-vcs)

;;; init-vcs.el ends here
