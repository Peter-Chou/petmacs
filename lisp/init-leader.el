;; init-leader.el --- Setup c/c++ IDE.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(use-package spaceleader
  :ensure nil
  :commands (
     leader/with-prefix
     leader/with-major-mode-prefix
     leader/without-prefix
     leader/declare-prefix
     leader/declare-prefix*
     leader/declare-prefix-for-mode
     leader/declare-prefix-for-mode*
     leader/declare-prefix-for-mode!
     leader/declare-prefix-for-mode!*
     leader/set-keys
     leader/set-keys-for-mode
     leader/set-keys-for-mode!))


(leader/declare-prefix*
  "a" "applications"
  "ao" "org"

  "f" "files"
  "fy" "copy"
  "fv" "variable"
  "fe" "emacs conf"
  "fC" "unix <-> dos"

  "F" "Frame"
  "g" "git"
  "gf" "magit file"
  "T" "Theme"
  "q" "quit"
  "i" "insert"
  "p" "project"
  "j" "jump"
  "e" "error"
  "b" "buffer"
  "B" "bookmarks"
  "n" "narrow"
  "o" "origami (code fold)"

  "w" "window"
  "wp" "popout window"
  )

(leader/set-keys
  "'"   'petmacs/pop-eshell
  "?"   'counsel-descbinds
  "/"   'counsel-projectile-rg
  "v"   'er/expand-region
  "u"   'universal-argument
  "d"   'xref-pop-marker-stack
 "TAB" 'petmacs/alternate-buffer)

(leader/with-prefix "a"
  (leader/set-keys
    "d"  'deer
    "p"  'list-processes
    "P"  'proced
    "r"  'ranger
    "k"  'paradox-list-packages
    "u"  'paradox-upgrade-packages
    "w"  'whitespace-cleanup
    )
  (leader/with-prefix "o"
    (leader/set-keys
      "a" 'org-agenda-list
      "c" 'org-capture
      "e" 'org-store-agenda-views
      "p" 'org-projectile/capture
      "o" 'org-agenda
      "l" 'org-store-link
      "m" 'org-tags-view
      "s" 'org-search-view
      "t" 'org-todo-list
      "/" 'org-occur-in-agenda-files
      ))
  )

(leader/with-prefix "f"
  (leader/set-keys
    "f"  'counsel-find-file
    "j"  'dired-jump
    "t"  'treemacs
    "B"  'treemacs-bookmark
    "T"  'treemacs-find-file
    "L"  'counsel-locate
    "r"  'counsel-buffer-or-recentf
    "R"  'petmacs/rename-current-buffer-file
    "s"  'save-buffer
    "S"  'evil-write-all
    "c"  'petmacs/copy-file
    "b"  'counsel-bookmark
    "B"  'treemacs-bookmark
    )
  (leader/with-prefix "y"
    (leader/set-keys
      "y" 'petmacs/copy-file-path
      "Y" 'petmacs/projectile-copy-file-path
      "d" 'petmacs/copy-directory-path
      "n" 'petmacs/copy-file-name
      ))
  (leader/with-prefix "v"
    (leader/set-keys
      "d" 'add-dir-local-variable
      "f" 'add-file-local-variable
      "p" 'add-file-local-variable-prop-line
      ))
  (leader/with-prefix "e"
    (leader/set-keys
      "o" 'petmacs/find-org-global-todos
      "c" 'petmacs/find-custom-file
      "d" 'petmacs/find-dotfile
      ))

  (leader/with-prefix "C"
    (leader/set-keys
      "u" 'dos2unix
      "d" 'unix2dos
      "r" 'petmacs/save-buffer-gbk-as-utf8
      ))
  )

(leader/with-prefix "F"
  (leader/set-keys
  "f" 'find-file-other-frame
  "d" 'delete-frame
  "D" 'delete-other-frames
  "b" 'switch-to-buffer-other-frame
  "B" 'display-buffer-other-frame
  "o" 'other-frame
  "O" 'dired-other-frame
  "n" 'make-frame)
  )

(leader/with-prefix "b"
  (leader/set-keys
  "b" 'ivy-switch-buffer
  "B" 'ibuffer
  "d" 'kill-this-buffer
  "n" 'next-buffer
  "p" 'previous-buffer
  "R" 'petmacs/revert-this-buffer
  "s" 'petmacs/goto-scratch-buffer
  "x" 'kill-buffer-and-window
  "h" 'petmacs/goto-dashboard
  "m" 'petmacs/switch-to-minibuffer-window
  "Y" 'petmacs/copy-whole-buffer-to-clipboard
  "a" 'persp-add-buffer
  "r" 'persp-remove-buffer
  "j" 'ace-window
  "t" 'imenu-list-smart-toggle
  "I" 'lsp-ui-imenu
  "i" 'imenu-list)
)

(leader/with-prefix "B"
  (leader/set-keys
  "s" 'bookmark-set
  "d" 'bookmark-delete
  "r" 'bookmark-rename
  "l" 'bookmark-bmenu-list)
)

(leader/with-prefix "n"
  (leader/set-keys
  "f" 'narrow-to-defun
  "r" 'narrow-to-region
  "p" 'narrow-to-page
  "w" 'widen)
)

(leader/with-prefix "w"
  (leader/set-keys
  "."  'hydra-frame-window/body
  "c"  'olivetti-mode
  "d"  'delete-window
  "D"  'ace-delete-window

  "pm" 'petmacs/shackle-popup-message-buffer
  "pc" 'petmacs/shackle-popup-compilation-buffer)
)

(leader/with-prefix "g"
  (leader/set-keys
  "c"  'magit-clone
  "s"  'magit-status
  "i"  'magit-init
  "l"  'magit-log-head
  "L"  'magit-list-repositories
  "m"  'magit-dispatch
  "S"  'magit-stage-file
  "U"  'magit-unstage-file
  "w" 'browse-at-remote

  ;;; magit file
  "ff" 'magit-find-file
  "fl" 'magit-log-buffer-file
  "fd" 'magit-diff)
)

(leader/with-prefix "j"
  (leader/set-keys
  "i" 'petmacs/counsel-jump-in-buffer
  "w" 'evil-avy-goto-word-or-subword-1
  "D" 'deer-jump-other-window
  "c" 'goto-last-change
  "d" 'deer
  "j" 'avy-goto-char-timer
  "J" 'avy-goto-char-2))

(leader/with-prefix "e"
  (leader/set-keys
  "b" 'flycheck-buffer
  "c" 'flycheck-clear
  "h" 'flycheck-describe-checker
  "l" 'petmacs/toggle-flycheck-error-list
  "n" 'petmacs/next-error
  "N" 'petmacs/previous-error
  "p" 'petmacs/previous-error
  "s" 'flycheck-select-checker
  "S" 'flycheck-set-checker-executable
  "v" 'flycheck-verify-setup
  "y" 'flycheck-copy-errors-as-kill
  "x" 'flycheck-explain-error-at-point))

(leader/with-prefix "p"
  (leader/set-keys
  "'"  'petmacs/projectile-pop-eshell
  "t"  'petmacs/treemacs-project-toggle
  "b"  'counsel-projectile-switch-to-buffer
  "d"  'counsel-projectile-find-dir
  "p"  'counsel-projectile-switch-project
  "f"  'counsel-projectile-find-file
  "r"  'projectile-recentf
  "o"  'org-projectile/goto-todos
  "l"  'petmacs/ivy-persp-switch-project
  "v"  'projectile-vc))

(leader/with-prefix "i"
  (leader/set-keys
  "s" 'ivy-yasnippet
  "f"  'insert-file))

(leader/with-prefix "T"
  (leader/set-keys
    "s"  'petmacs/select-theme
    "n"  'petmacs/cycle-theme))

(leader/with-prefix "q"
  (leader/set-keys
    "q" 'petmacs/frame-killer
    "Q" 'kill-emacs
    "h" 'suspend-frame
    "R" 'restart-emacs))


(leader/with-prefix "o"
  (leader/set-keys
    "o."  'origami-hydra/body
    ))

;;; major mode keybinidngs

;;; lsp major mode settings
(dolist (mode petmacs-lsp-active-modes)
  (leader/declare-prefix-for-mode!* mode
    "g" "goto"
    "G" "goto (other window)"
    "p" "peek"
    "b" "backend"
    "r" "refactor"
    "F" "folders"
    "d" "debug"
    )

  (leader/set-keys-for-mode! mode
    ;; format
    "rb" #'lsp-format-buffer
    "rr" #'lsp-format-region
    "rR" #'lsp-rename
    ;; goto
    "gr" #'lsp-find-references
    "gt" #'lsp-find-type-definition
    "gd" #'xref-find-definitions
    "gD" #'lsp-find-declaration
    "gi" #'lsp-find-implementation
    "gk" #'petmacs/lsp-avy-goto-word
    "gK" #'petmacs/lsp-avy-goto-symbol
    "gM" #'lsp-ui-imenu
    ;; goto other window
    "Gr" #'petmacs/lsp-find-references-other-window
    "Gt" #'petmacs/lsp-find-type-definition-other-window
    "Gd" #'petmacs/lsp-find-definition-other-window
    "GD" #'petmacs/lsp-find-declaration-other-window
    "Gi" #'petmacs/lsp-find-implementation-other-window
    ;; peek
    "pd" #'lsp-ui-peek-find-definitions
    "pi" #'lsp-ui-peek-find-implementation
    "pr" #'lsp-ui-peek-find-references
    "pRn" #'lsp-ui-find-next-reference
    "pRp" #'lsp-ui-find-prev-reference
    ;; backend
    "ba" #'lsp-execute-code-action
    "bd" #'lsp-describe-session
    "br" #'lsp-restart-workspace
    "bs" #'lsp-shutdown-workspace

    "d." #'dap-hydra
    ;; toggles
    ;; "Td" #'lsp-ui-doc-mode
    ;; "Ts" #'lsp-ui-sideline-mode
    ;; "TF" #'petmacs/lsp-ui-doc-func
    ;; "TS" #'petmacs/lsp-ui-sideline-symb
    ;; "TI" #'petmacs/lsp-ui-sideline-ignore-duplicate
    ;; "Tl" #'lsp-lens-mode
    ;; folders
    "Fs" #'lsp-workspace-folders-switch
    "Fr" #'lsp-workspace-folders-remove
    "Fa" #'lsp-workspace-folders-add))


(leader/declare-prefix-for-mode!* 'python-mode
  "c" "compile"
  "v" "virtual environment"
  "vp" "pipenv"
  "t" "test"
  )

(leader/set-keys-for-mode! 'python-mode
  "cc" 'petmacs/python-execute-file
  "cC" 'petmacs/python-execute-file-focus
  "ck" 'petmacs/quit-subjob

  "rI" 'py-isort-buffer
  "ri" 'petmacs/python-remove-unused-imports
  "rB" 'yapfify-buffer

  "sB" 'petmacs/python-shell-send-buffer-switch
  "sb" 'petmacs/python-shell-send-buffer
  "sb" 'petmacs/python-shell-send-buffer
  "sF" 'petmacs/python-shell-send-defun-switch
  "sf" 'petmacs/python-shell-send-defun
  "si" 'petmacs/python-start-or-switch-repl
  "sr" 'petmacs/python-shell-send-region
  "sR" 'petmacs/python-shell-send-region-switch
  "sk" 'petmacs/python-interrupt-repl
  "sq" 'petmacs/python-quit-repl

  "va" 'pyvenv-activate
  "vd" 'pyvenv-deactivate
  "vw" 'pyvenv-workon
  "vpa" 'pipenv-activate
  "vpd" 'pipenv-deactivate
  "vpi" 'pipenv-install
  "vpo" 'pipenv-open
  "vps" 'pipenv-shell
  "vpu" 'pipenv-uninstall)

(provide 'init-leader)

;;; init-leader.el ends here
