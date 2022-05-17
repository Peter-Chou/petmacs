;; init-keybindings.el --- Better default configurations.	-*- lexical-binding: t -*-

(require 'init-funcs)
(require 'core-funcs)

(global-set-key (kbd "C-SPC") 'nil)

(use-package spaceleader
  :quelpa
  (spaceleader :fetcher github
  	           :repo "mohkale/spaceleader"
  	           :files ("*"))
  :init
  (setq leader-nnorm-key "M-m"
        leader-key "<SPC>"
        leader-major-mode-key "\,"))

(leader-set-keys
  ;; "'"   #'petmacs/shell-pop
  "/"   #'consult-ripgrep

  "v"   #'er/expand-region
  "u"   #'universal-argument
  "d"   #'xref-pop-marker-stack
  "TAB"  #'petmacs/alternate-buffer
  )

(leader-declare-prefix
  "a" "apps"
  "ao" "org")
(leader-with-prefix "a"
  (leader-set-keys
    "d"  #'deer
    "p"  #'list-processes
    "P"  #'proced
    "r"  #'ranger
    "k"  #'paradox-list-packages
    "u"  #'paradox-upgrade-packages
    "w"  #'whitespace-cleanup

    ;; "ao" prefix
    "oa" #'org-agenda-list
    "oc" #'org-capture
    "oe" #'org-store-agenda-views
    "op" #'org-projectile/capture
    "oo" #'org-agenda
    "ol" #'org-store-link
    "om" #'org-tags-view
    "os" #'org-search-view
    "ot" #'org-todo-list
    "o/" #'org-occur-in-agenda-files
    ))

(leader-declare-prefix
  "f" "files"
  "fy" "copy"
  "fv" "variable"
  "fe" "emacs conf"
  "fC" "unix <-> dos")
(leader-with-prefix "f"
  (leader-set-keys
    "f" #'find-file
	"w"  #'find-file-other-window
    "F"  #'find-file-other-frame
    "j"  #'dired-jump
    "t"  #'treemacs
    "T"  #'treemacs-find-file
    "B"  #'treemacs-bookmark
    "L"  #'consult-locate
    "r"  #'consult-recent-file
    "R"  #'petmacs/rename-current-buffer-file
	"s"  #'save-buffer
    "S"  #'evil-write-all
    "d"  #'petmacs/delete-this-file
    "c"  #'petmacs/copy-file
    "b"  #'consult-bookmark
    "B"  #'treemacs-bookmark

    "m" #'consult-mark
    "M" #'consult-global-mark

	;; "fy" prefix
    "yy" #'petmacs/copy-file-path
    "yY" #'petmacs/projectile-copy-file-path
	"yd" #'petmacs/copy-directory-path
    "yn" #'petmacs/copy-file-name

    ;; "fv" prefix
    "vd" #'add-dir-local-variable
    "vf" #'add-file-local-variable
    "vp" #'add-file-local-variable-prop-line

	;; "fC" prefix
    "Cu" #'dos2unix
    "Cd" #'unix2dos
    "Cr" #'save-buffer-gbk-as-utf8

	;; "fe" prefix
    "eo" #'petmacs/goto-org-global-todos
    "ed" #'petmacs/find-dotfile
    "er" #'petmacs/reload-init-file
    ))


(leader-declare-prefix
  "b" "buffer")
(leader-with-prefix "b"
  (leader-set-keys
    "b"  #'consult-buffer
    "I" #'ibuffer
    "b"  #'consult-buffer-other-window
	"d" #'kill-this-buffer
    "n" #'next-buffer
    "p" #'previous-buffer
    "R" #'petmacs/revert-this-buffer
    "s" #'petmacs/goto-scratch-buffer
    "x" #'kill-buffer-and-window
    "h" #'petmacs/goto-dashboard
    "m" #'petmacs/switch-to-minibuffer-window
    "Y" #'petmacs/copy-whole-buffer-to-clipboard
	"a" #'persp-add-buffer
    "r" #'persp-remove-buffer
    "j" #'ace-window
    "t" #'imenu-list-smart-toggle
    "i" #'imenu-list

    ))

(leader-declare-prefix
  "B" "bookmark")
(leader-with-prefix "B"
  (leader-set-keys
    "b" #'consult-bookmark
    "s" #'bookmark-set
    "d" #'bookmark-delete
    "r" #'bookmark-rename
    "l" #'bookmark-bmenu-list
    ))

(leader-declare-prefix
  "t" "toggle")
(leader-with-prefix "t"
  (leader-set-keys
    "-" #'centered-cursor-mode
    "'" #'petmacs/open-gnome-terminal
    "s" #'flycheck-mode
    "l" #'display-fill-column-indicator-mode
    "n" #'display-line-numbers-mode
    "f" #'focus-mode
    "F" #'toggle-frame-fullscreen
    "x" #'read-only-mode
    "M" #'maximize-window
    "t" #'consult-theme

    ;; "tp" prefix
    "p" #'proxy-http-toggle
    ))

(leader-declare-prefix
  "F" "frame")
(leader-with-prefix "F"
  (leader-set-keys
    ;; "f" #'find-file-other-frame
    "f" #'consult-buffer-other-frame
    "d" #'delete-frame
    "D" #'delete-other-frames
    "b" #'switch-to-buffer-other-frame
    "B" #'display-buffer-other-frame
    "o" #'other-frame
    "O" #'dired-other-frame
    "n" #'make-frame
    "'" #'petmacs//vterm-other-window
    "m" #'petmacs-frame-maximize
    "r" #'petmacs-frame-restore
    "H" #'petmacs-frame-left-half
    "L" #'petmacs-frame-right-half
    "K" #'petmacs-frame-top-half
    "J" #'petmacs-frame-bottom-half
    ))

(leader-declare-prefix
  "w" "window")
(leader-with-prefix "w"
  (leader-set-keys
    "."  #'hydra-frame-window/body
    "j" #'ace-window
    "c"  #'olivetti-mode
    "r"  #'winner-undo
    "d"  #'delete-window
    "D"  #'ace-delete-window
    "o" #'toggle-one-window

    ;; "wp" prefix
    ))

(leader-declare-prefix
  "g" "git"
  "gf" "file")
(leader-with-prefix "g"
  (leader-set-keys
    "c"  #'magit-clone
    "s"  #'magit-status
    "i"  #'magit-init
    "l"  #'magit-log-head
    "L"  #'magit-list-repositories
    "m"  #'magit-dispatch
    "S"  #'magit-stage-file
    "U"  #'magit-unstage-file

  ;;; "gf" file
    "ff" #'magit-find-file
    "fl" #'magit-log-buffer-file
    "fd" #'magit-diff
    ))

(leader-declare-prefix
  "j" "jump")
(leader-with-prefix "j"
  (leader-set-keys
    "i" #'consult-imenu
    "w" #'evil-avy-goto-word-or-subword-1
    "D" #'deer-jump-other-window
    "c" #'goto-last-change
    "d" #'deer
    "j" #'avy-goto-char-timer
    "J" #'avy-goto-char-2
    "t" #'magit-todos-list
    "y" #'consult-yank-pop
    ))

(leader-declare-prefix
  "e" "error")
(leader-with-prefix "e"
  (leader-set-keys
    "b" #'flycheck-buffer
    "c" #'flycheck-clear
    "h" #'flycheck-describe-checker
    "l" #'petmacs/toggle-flycheck-error-list
    "n" #'petmacs/next-error
    "N" #'petmacs/previous-error
    "p" #'petmacs/previous-error
    "s" #'flycheck-select-checker
    "S" #'flycheck-set-checker-executable
    "v" #'flycheck-verify-setup
    "y" #'flycheck-copy-errors-as-kill
    "x" #'flycheck-explain-error-at-point
    ))

(leader-declare-prefix
  "p" "project")
(leader-with-prefix "p"
  (leader-set-keys
    ;; "'"  #'petmacs/projectile-pop-eshell
    "'"  #'petmacs/projectile-shell-pop
    ;;     (leader-set-keys "p'"   #'petmacs/projectile-pop-vterm)
    "t"  #'petmacs/treemacs-project-toggle
    "b"  #'consult-projectile-switch-to-buffer
    "d"  #'consult-projectile-find-dir
    "p"  #'consult-projectile-switch-project
    "f"  #'consult-projectile-find-file
    "r"  #'consult-projectile-recentf
    "o"  #'org-projectile/goto-todos
    "v"  #'projectile-vc
    ))


(provide 'init-keybindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-keybindings.el ends here
