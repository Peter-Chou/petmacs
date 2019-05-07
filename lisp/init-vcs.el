;; init-vcs.el --- Setup version control.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package magit
  ;; :pin melpa-stable
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
  (git-timemachine-minibuffer-author-face ((t (:inherit font-lock-string-face))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
  :bind (:map vc-prefix-map
		("t" . git-timemachine)))

;; Pop up last commit information of current line
(use-package git-messenger
  :bind (:map vc-prefix-map
		("p" . git-messenger:popup-message)
		:map git-messenger-map
		("m" . git-messenger:copy-message))
  :init
  ;; Use magit-show-commit for showing status/diff commands
  (setq git-messenger:use-magit-popup t))

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

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:modified-sign " ")
  (git-gutter:added-sign    "+")
  (git-gutter:deleted-sign  "-")
  :custom-face
  (git-gutter:modified ((t (:background "#f1fa8c"))))
  (git-gutter:added    ((t (:background "#50fa7b"))))
  (git-gutter:deleted  ((t (:background "#ff79c6"))))
  :init
  (run-with-idle-timer 1 nil 'global-git-gutter-mode)
  (setq git-gutter:update-interval 2
        ;; git-gutter:modified-sign " "
        ;; git-gutter:added-sign "+"
        ;; git-gutter:deleted-sign "-"
        ;; git-gutter:diff-option "-w"
        git-gutter:ask-p nil
        git-gutter:verbosity 0
        git-gutter:handled-backends '(git hg bzr svn)
        git-gutter:hide-gutter t))

(use-package evil-magit
  ;; :pin melpa-stable
  :init
  (with-eval-after-load 'magit
    (require 'evil-magit)
    (add-hook 'git-commit-mode-hook 'evil-insert-state)))

;; Show tasks
(use-package pcre2el) ;; magit-todos dependency
(use-package magit-todos
  :commands (magit-todos-mode magit-todos-list magit-todos-list-internal)
  :quelpa (magit-todos :repo "Peter-Chou/magit-todos" :fetcher github)
  :hook (after-init . magit-todos-mode)
  :init
  ;; disable magit-todos keybindings which is not compatible with evil-magit
  (setq magit-todos-section-map
	(let ((map (make-sparse-keymap)))
	  (define-key map "j" #'evil-next-visual-line)
	  (define-key map "k" #'evil-previous-visual-line)
	  map))
  :config
  (when sys/win32p
    ;; windows can only use grep
    (setq magit-todos-nice nil)
    (setq magit-todos-scanner 'magit-todos--scan-with-git-grep)))

;; Git related modes
(use-package gitattributes-mode)
(use-package gitignore-mode)
(use-package gitconfig-mode)
(use-package git-commit)

(provide 'init-vcs)

;;; init-vcs.el ends here
