;; init-git.el --- Better default configurations.	-*- lexical-binding: t -*-

(use-package magit
  ;; :pin melpa-stable
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

;; Show TODOs in magit
(use-package magit-todos
  :init
  (setq magit-todos-nice (if (executable-find "nice") t nil))
  (setq magit-todos-exclude-globs '("third_party"))
  (let ((inhibit-message t))
    (magit-todos-mode 1))
  :config
  (with-eval-after-load 'magit-status
    (transient-append-suffix 'magit-status-jump '(0 0 -1)
      '("t " "Todos" magit-todos-jump-to-todos))))

;; Git related modes
(use-package git-modes)

(provide 'init-git)
