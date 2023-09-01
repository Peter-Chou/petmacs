;; -*- lexical-binding: t no-byte-compile: t -*-

(require 'init-custom)
(require 'init-funcs)
(require 'core-funcs)

(when sys/wslp
  ;; windows端将left windows 键 -> Application 键
  (global-set-key (kbd "<menu>") nil)
  (define-key key-translation-map (kbd "<menu>") 'event-apply-super-modifier)

  (global-set-key (kbd "s-a") 'mark-whole-buffer) ;;对应Windows上面的Ctrl-a 全选
  (global-set-key (kbd "s-c") 'kill-ring-save) ;;对应Windows上面的Ctrl-c 复制
  (global-set-key (kbd "s-s") 'save-buffer) ;; 对应Windows上面的Ctrl-s 保存
  (global-set-key (kbd "s-v") 'yank) ;对应Windows上面的Ctrl-v 粘贴
  (global-set-key (kbd "s-z") 'undo) ;对应Windows上面的Ctrol-z 撤销
  (global-set-key (kbd "s-x") 'kill-region) ;对应Windows上面的Ctrol-x 剪切
  )

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
  "'"   #'multi-vterm
  ;; "/"   #'consult-ripgrep
  "/"   #'affe-grep

  "v"   #'er/expand-region
  "u"   #'universal-argument
  ;; "d"   #'xref-pop-marker-stack
  "TAB"  #'petmacs/alternate-buffer
  )


(leader-declare-prefix
  "z" "fold/zip")
(leader-with-prefix "z"
  (leader-set-keys
    "."  #'ts-fold-hydra/body
    ))

(leader-declare-prefix
  "a" "apps"
  "ao" "org"
  "at" "pomodoro"
  )
(leader-with-prefix "a"
  (leader-set-keys
    "d"  #'deer
    "p"  #'list-processes
    "e" #'list-environment
    "P"  #'proced
    "k"  #'paradox-list-packages
    "u"  #'paradox-upgrade-packages
    "w"  #'whitespace-cleanup

    ;; "ao" prefix
    "oa" #'org-agenda-list
    "oc" #'org-capture
    "oe" #'org-store-agenda-views
    "op" #'org-projectile-capture-for-current-project
    "oo" #'org-agenda
    "ol" #'org-store-link
    "om" #'org-tags-view
    "os" #'org-search-view
    "ot" #'org-todo-list
    "o/" #'org-occur-in-agenda-files

    ;; "at" prefix
    "tt" #'petmacs/pomodoro-toggle
    "tp" #'pomodoro-pause
    "tr" #'pomodoro-resume
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
    "es" #'petmacs/goto-org-global-schedules
    "et" #'petmacs/goto-org-global-todos
    "ed" #'petmacs/find-dotfile
    "er" #'petmacs/reload-init-file
    ))

(leader-declare-prefix
  "b" "buffer")
(leader-with-prefix "b"
  (leader-set-keys
    "b" #'consult-buffer
    "B" #'consult-buffer-other-window
    "I" #'ibuffer
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
    "i" #'imenu-list-smart-toggle
    ))

(leader-declare-prefix
  "l" "layout")
(leader-with-prefix "l"
  (leader-set-keys
    "n" #'persp-next
    "p" #'persp-prev
    "s" #'persp-frame-switch
    "S" #'persp-window-switch
    "r" #'persp-rename
    "c" #'persp-copy
    "C" #'persp-kill
    "z" #'persp-save-and-kill
    "a" #'persp-add-buffer
    "b" #'persp-switch-to-buffer
    "t" #'persp-temporarily-display-buffer
    "i" #'persp-import-buffers
    "I" #'persp-import-win-conf
    "k" #'persp-remove-buffer
    "K" #'persp-kill-buffer
    "w" #'persp-save-state-to-file
    "W" #'persp-save-to-file-by-names
    "l" #'persp-load-state-from-file
    "L" #'persp-load-from-file-by-names
    "o" #'petmacs/disable-persp-mode
    ))

(leader-declare-prefix
  "w" "window")
(leader-with-prefix "w"
  (leader-set-keys
    "."  #'ace-window-hydra/body
    "j" #'ace-window
    "c"  #'writeroom-mode
    "r"  #'winner-undo
    "d"  #'delete-window
    "D"  #'ace-delete-window
    "o" #'toggle-one-window
    "t" #'popper-toggle-type
    "p" #'popper-cycle

    ;; "wp" prefix
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
  "t" "toggle"
  "tt" "theme")
(leader-with-prefix "t"
  (leader-set-keys
    "-" #'centered-cursor-mode
    "'" #'petmacs/open-gnome-terminal
    "s" #'flymake-mode
    "l" #'display-fill-column-indicator-mode
    "n" #'display-line-numbers-mode
    "f" #'focus-mode
    "F" #'toggle-frame-fullscreen
    "x" #'read-only-mode
    "m" #'minimap-mode
    "M" #'maximize-window
    "c" #'prettify-symbols-mode

    "z" #'writeroom-mode

    "tt" #'modus-themes-toggle
    "ts" #'petmacs/consult-theme

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
    "fm" 'magit-file-dispatch
    ))

(leader-declare-prefix
  "j" "jump")
(leader-with-prefix "j"
  (leader-set-keys
    "i" #'consult-imenu
    "w" #'avy-goto-word-or-subword-1
    "D" #'dired-jump-other-window
    "c" #'goto-last-change
    "d" #'deer
    "j" #'avy-goto-char
    "J" #'avy-goto-char-2
    "l" #'avy-goto-line
    "t" #'magit-todos-list
    "y" #'consult-yank-pop
    ))

(leader-declare-prefix
  "e" "error")
(leader-with-prefix "e"
  (leader-set-keys
    ;; "l" #'consult-flymake
    "l" #'flycheck-list-errors
    ;; "b" #'flymake-diagnostic-buffer
    ;; "c" #'flycheck-clear
    ;; "h" #'flycheck-describe-checker
    ;; "n" #'petmacs/next-error
    ;; "N" #'petmacs/previous-error
    ;; "p" #'petmacs/previous-error
    ;; "s" #'flycheck-select-checker
    ;; "S" #'flycheck-set-checker-executable
    ;; "v" #'flycheck-verify-setup
    ;; "y" #'flycheck-copy-errors-as-kill
    ;; "x" #'flycheck-explain-error-at-point
    ))

(leader-declare-prefix
  "p" "project")
(leader-with-prefix "p"
  (leader-set-keys
    ;; "'"  #'petmacs/projectile-pop-eshell
    "."  #'consult-project-extra-find
    ;; "'"  #'petmacs/projectile-shell-pop
    "'"  #'multi-vterm-project
    "t"  #'petmacs/treemacs-project-toggle
    "b"  #'consult-projectile-switch-to-buffer
    "d"  #'consult-projectile-find-dir
    "p"  #'consult-projectile-switch-project
    ;; "f"  #'consult-projectile-find-file
    "f"  #'affe-find
    "r"  #'consult-projectile-recentf
    "o"  #'org-projectile/goto-project-todos
    "c"  #'org-projectile-project-todo-completing-read
    "v"  #'projectile-vc
    ))

(leader-declare-prefix
  "i" "insert")
(leader-with-prefix "i"
  (leader-set-keys
    "s" #'consult-yasnippet
    "f" #'insert-file
    ))

(leader-declare-prefix
  "q" "quit")
(leader-with-prefix "q"
  (leader-set-keys
    "q" #'petmacs/frame-killer
    "Q" #'kill-emacs
    "h" #'suspend-frame
    "R" #'restart-emacs
    ))

(leader-declare-prefix
  "d" "debug"
  "db"  "breakpoints"
  "dd"  "debugging"
  "de"  "eval"
  "dI"  "inspect"
  "dS"  "switch"
  "dw"  "debug windows")
(leader-with-prefix "d"
  (leader-set-keys
    "." #'dap-hydra
    ;; repl
    "'"  #'dap-ui-repl
    ;; abandon
    "a"  #'dap-disconnect
    "A"  #'dap-delete-all-sessions
    ;; breakpoints
    "bb" #'dap-breakpoint-toggle
    "bc" #'dap-breakpoint-condition
    "bl" #'dap-breakpoint-log-message
    "bh" #'dap-breakpoint-hit-condition
    "ba" #'dap-breakpoint-add
    "bd" #'dap-breakpoint-delete
    "bD" #'dap-breakpoint-delete-all
    ;; debuging/running
    "dd" #'dap-debug
    "de" #'dap-debug-edit-template
    "dl" #'dap-debug-last
    "dr" #'dap-debug-recent
    ;; eval
    "ee" #'dap-eval
    "er" #'dap-eval-region
    "et" #'dap-eval-thing-at-point
    "et" #'dap-ui-expressions-add
    ;; inspect
    "Ii" #'dap-ui-inspect
    "Ir" #'dap-ui-inspect-region
    "It" #'dap-ui-inspect-thing-at-point
    ;; stepping
    "c"  #'dap-continue
    "i"  #'dap-step-in
    "o"  #'dap-step-out
    "r"  #'dap-restart-frame
    "s"  #'dap-next
    "v"  #'dap-ui-inspect-thing-at-point
    ;; switching
    "Ss" #'dap-switch-session
    "St" #'dap-switch-thread
    "Sf" #'dap-switch-frame
    ;; windows
    "wo" #'dap-go-to-output-buffer
    "wl" #'dap-ui-locals
    "ws" #'dap-ui-sessions
    "wb" #'dap-ui-breakpoints
    ))

;;; major mode keybinidngs ;;;;;;;;;;;;;;;;;;

;;; lsp for major mode

(cond ((equal petmacs-lsp-client 'lsp-bridge-mode)
       (dolist (mode '(c-mode c++-mode cmake-mode java-mode python-mode ruby-mode lua-mode rust-mode rustic-mode erlang-mode elixir-mode go-mode haskell-mode haskell-literate-mode dart-mode scala-mode typescript-mode typescript-tsx-mode js2-mode js-mode rjsx-mode tuareg-mode latex-mode Tex-latex-mode texmode context-mode texinfo-mode bibtex-mode clojure-mode clojurec-mode clojurescript-mode clojurex-mode sh-mode web-mode css-mode elm-mode emacs-lisp-mode ielm-mode lisp-interaction-mode org-mode php-mode yaml-mode zig-mode groovy-mode dockerfile-mode d-mode f90-mode fortran-mode nix-mode ess-r-mode verilog-mode))
         (leader-declare-prefix-for-major-mode mode
           "=" "format"
           "a" "code actions"
           "g" "goto"
           "G" "goto (other window)"
           "p" "peek"
           "w" "workspace"
           "r" "refactor"
           "h" "help")
         (leader-set-keys-for-major-mode mode
           ;; format
           "==" #'lsp-bridge-code-format

           ;; code actions
           "aa" #'lsp-bridge-code-action

           ;; format
           "rr" #'lsp-bridge-rename

           ;; goto
           "gd" #'petmacs/lsp-bridge-jump
           "ga" #'consult-apropos
           "ge" #'lsp-bridge-list-diagnostics
           "gr" #'lsp-bridge-find-references
           "gi" #'lsp-bridge-find-impl
           "gt" #'lsp-bridge-find-define

           "gb" #'petmacs/lsp-bridge-jump-back
           "gf" #'xref-find-definitions-other-frame

           ;; goto other window
           "Gd" #'lsp-bridge-find-def-other-window
           "Gi" #'lsp-bridge-find-impl-other-window

           ;; workspace
           "wr" #'lsp-bridge-restart-process

           ;; help
           "hh" #'lsp-bridge-lookup-documentation
           "hs" #'lsp-bridge-signature-help-fetch)))
      (t
       (dolist (mode petmacs-lsp-active-modes)
         (leader-declare-prefix-for-major-mode mode
           "=" "format"
           "a" "code actions"
           "g" "goto"
           "G" "goto (other window)"
           "p" "peek"
           "w" "workspace"
           "t" "toggle module"
           "r" "refactor"
           "h" "help"
           "F" "folders"
           "x" "text/code")
         (leader-set-keys-for-major-mode mode
           ;; format
           "==" #'lsp-format-buffer
           "=r" #'lsp-format-region

           ;; code actions
           "aa" #'lsp-execute-code-action
           "ah" #'lsp-document-highlight
           "al" #'lsp-avy-lens

           ;; format
           "ri" #'lsp-organize-imports
           "rb" #'lsp-format-buffer
           "rr" #'lsp-rename

           ;; goto
           "gd" #'lsp-find-definition
           "gD" #'lsp-find-declaration
           "ge" #'lsp-treemacs-errors-list
           "gh" #'lsp-treemacs-call-hierarchy
           "gr" #'lsp-find-references
           "gi" #'lsp-find-implementation
           "gt" #'lsp-find-type-definition
           "gs" #'consult-lsp-file-symbols
           "gS" #'consult-lsp-symbols
           "gm" #'symbols-outline-show

           "gb" #'xref-pop-marker-stack
           "gf" #'xref-find-definitions-other-frame

           ;; goto other window
           "Gd" #'petmacs/lsp-find-definition-other-window
           "GD" #'petmacs/lsp-find-declaration-other-window
           "Gi" #'petmacs/lsp-find-implementation-other-window
           "Gt" #'petmacs/lsp-find-type-definition-other-window
           "Gr" #'petmacs/lsp-find-references-other-window

           ;; peek
           "pd" #'lsp-ui-peek-find-definitions
           "pi" #'lsp-ui-peek-find-implementation
           "pr" #'lsp-ui-peek-find-references
           "ps" #'lsp-ui-peek-find-workspace-symbol
           "pe" #'lsp-ui-flycheck-list
           "pb" #'lsp-ui-peek-jump-backward
           "pn" #'lSp-ui-peek-jump-forward
           "pRn" #'lsp-ui-find-next-reference
           "pRp" #'lsp-ui-find-prev-reference

           ;; workspace
           "wd" #'lsp-describe-session
           "wD" #'lsp-disconnect
           "wq" #'lsp-workspace-shutdown
           "wr" #'lsp-restart-workspace
           "wv" #'lsp-version
           "ws" #'lsp

           ;; help
           "hh" #'lsp-describe-thing-at-point
           "hs" #'lsp-signature-activate
           "hg" #'lsp-ui-doc-glance

           ;; text/code
           "xh" #'lsp-document-highlight
           "xl" #'lsp-lens-show
           "xL" #'lsp-lens-hide

           ;; folders
           "Fs" #'lsp-workspace-folders-switch
           "Fr" #'lsp-workspace-folders-remove
           "Fb" #'lsp-workspace-blacklist-remove
           "Fa" #'lsp-workspace-folders-add

           ;; toggles
           "tD" #'lsp-modeline-diagnostics-mode
           "tL" #'lsp-toggle-trace-io
           "tS" #'lsp-ui-sideline-mode
           "tT" #'lsp-treemacs-sync-mode
           "ta" #'lsp-modeline-code-actions-mode
           "tb" #'lsp-headerline-breadcrumb-mode
           "td" #'lsp-ui-doc-mode
           "tf" #'lsp-toggle-on-type-formatting
           "th" #'lsp-toggle-symbol-highlight
           "tl" #'lsp-lens-mode
           "ts" #'lsp-toggle-signature-auto-activate
           )))
      )

;;; python mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dolist (mode '(python-mode python-ts-mode))
  (leader-declare-prefix-for-major-mode mode
    "c" "compile"
    "v" "virtual environment"
    "t" "test")

  (leader-set-keys-for-major-mode mode
    "cc" #'petmacs/python-execute-file
    "ck" #'petmacs/quit-subjob

    "=i" #'py-isort-buffer
    "=I" #'petmacs/python-remove-unused-imports
    "rb" #'yapfify-buffer

    "va" #'pyvenv-activate
    "vd" #'pyvenv-deactivate
    "vw" #'pyvenv-workon
    ))

;;; emacs lisp mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
  (leader-declare-prefix-for-major-mode mode
    "c" "compile"
    "e" "eval"
    "t" "test"
    )

  (leader-set-keys-for-major-mode mode
    "'"  #'ielm
    "si" #'ielm
    "cc" #'emacs-lisp-byte-compile
    "eb" #'eval-buffer
    "ee" #'eval-last-sexp
    "er" #'eval-region
    "ef" #'eval-defun
    "tq" #'ert))


(provide 'init-keybindings)
