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

;; Show TODOs in magit
(use-package magit-todos
  :defines magit-todos-nice
  :init
  (setq magit-todos-nice (if (executable-find "nice") t nil))
  (setq magit-todos-exclude-globs '("third_party"))
  (let ((inhibit-message t))
    (magit-todos-mode 1))
  :config
  (with-eval-after-load 'magit-status
    (transient-append-suffix 'magit-status-jump '(0 0 -1)
      '("t " "Todos" magit-todos-jump-to-todos))))

;; Walk through git revisions of a file
(use-package git-timemachine
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit success))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
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

;; Git related modes
(use-package git-modes)

(provide 'init-git)
