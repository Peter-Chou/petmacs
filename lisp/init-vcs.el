;; -*- lexical-binding: t no-byte-compile: t -*-

(use-package magit
  :init
  (setq magit-diff-refine-hunk t
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

;; Access Git forges from Magit
(use-package forge
  :demand t
  :defines (forge-database-connector forge-topic-list-columns)
  :custom-face
  (forge-topic-label ((t (:inherit variable-pitch :height 0.9 :width condensed :weight regular :underline nil))))
  :init
  (setq forge-database-connector (if (and (require 'emacsql-sqlite-builtin nil t)
                                          (functionp 'emacsql-sqlite-builtin)
                                          (functionp 'sqlite-open))
                                     'sqlite-builtin
                                   'sqlite)
        forge-topic-list-columns
        '(("#" 5 forge-topic-list-sort-by-number (:right-align t) number nil)
          ("Title" 60 t nil title  nil)
          ("State" 6 t nil state nil)
          ("Updated" 10 t nil updated nil))))

;; Display transient in child frame
(when (childframe-completion-workable-p)
  (use-package transient-posframe
    :diminish
    :defines posframe-border-width
    :custom-face
    (transient-posframe ((t (:inherit tooltip))))
    (transient-posframe-border ((t (:inherit posframe-border :background unspecified))))
    :hook (after-init . transient-posframe-mode)
    :init
    (setq transient-posframe-border-width posframe-border-width
          transient-posframe-min-height nil
          transient-posframe-min-width 80
          transient-posframe-poshandler 'posframe-poshandler-frame-center
          transient-posframe-parameters '((left-fringe . 8)
                                          (right-fringe . 8)))
    :config
    (with-no-warnings
      (defun my-transient-posframe--hide ()
        "Hide transient posframe."
        (posframe-hide transient--buffer-name))
      (advice-add #'transient-posframe--delete :override #'my-transient-posframe--hide))))

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
              git-messenger:use-magit-popup t))

;; Resolve diff3 conflicts
(use-package smerge-mode
  :ensure nil
  :diminish
  :pretty-hydra
  ((:title (pretty-hydra-title "Smerge" 'octicon "diff")
    :color pink :quit-key "q")
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

;; Git related modes
(use-package git-modes)

(provide 'init-vcs)
