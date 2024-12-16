;; -*- lexical-binding: t no-byte-compile: t -*-

(eval-when-compile
  (require 'init-custom)
  (require 'init-funcs))

;; Make <escape> quit as much as possible
(define-key minibuffer-local-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-ns-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-completion-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map (kbd "<escape>") 'keyboard-escape-quit)

(define-key minibuffer-local-map (kbd "C-n") 'next-line-or-history-element)
(define-key minibuffer-local-map (kbd "C-p") 'previous-line-or-history-element)
(define-key minibuffer-local-map (kbd "M-n") 'next-line-or-history-element)
(define-key minibuffer-local-map (kbd "M-p") 'previous-line-or-history-element)

(define-key evil-normal-state-map (kbd "C-w o") #'petmacs/toggle-maximize-buffer)
(define-key evil-motion-state-map (kbd "C-w o") #'petmacs/toggle-maximize-buffer)

(define-key org-mode-map (kbd "C-j") #'org-priority-down)
(define-key org-mode-map (kbd "C-k") #'org-priority-up)

;; (define-key winum-keymap (kbd "M-9") 'petmacs/symbols-outline-smart-toggle)
(define-key winum-keymap (kbd "M-9") 'symbols-outline-show)

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
  :ensure nil
  :init
  (require 'spaceleader)
  (setq leader-nnorm-key "M-m"
        leader-key "<SPC>"
        leader-major-mode-key "\,"))

(leader-set-keys
  "'"   #'multi-vterm
  "/"   #'consult-ripgrep

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
  "s" "search/symbol")
(leader-with-prefix "s"
  (leader-set-keys
    "o"  #'symbol-overlay-hydra/body
    "O"  #'symbol-overlay-remove-all))


(leader-declare-prefix
  "c" "change")
(leader-with-prefix "c"
  (leader-set-keys
    "e"  #'remove-dos-eol))

(leader-declare-prefix
  "a" "apps"
  "ao" "org"
  "at" "pomodoro"
  )
(leader-with-prefix "a"
  (leader-set-keys
    "d"  #'deer
    "P"  #'list-processes
    "e" #'list-environment
    "P"  #'proced
    "w"  #'whitespace-cleanup

    ;; "ao" prefix
    "o#" #'org-agenda-list-stuck-projects
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
    "ofi" 'org-feed-goto-inbox
    "ofu" 'org-feed-update-all

    "oCc" 'org-clock-cancel
    "oCg" 'org-clock-goto
    "oCi" 'org-clock-in
    "oCI" 'org-clock-in-last
    "oCo" 'org-clock-out
    "oCr" 'org-resolve-clocks

    ;; "ap" prefix
    "pt" #'petmacs/pomodoro-toggle
    "pp" #'pomodoro-pause
    "pr" #'pomodoro-resume
    ))

(leader-declare-prefix
  "f" "files"
  "fy" "Yank/copy"
  "fv" "variable"
  "fe" "emacs conf"
  "fC" "unix <-> dos")
(leader-with-prefix "f"
  (leader-set-keys
    "A" #'find-alternate-file
    "f" #'find-file
    "c" #'petmacs/save-as
    ;; "c"  #'petmacs/copy-file
    "d"  #'petmacs/delete-this-file
    "D"  #'petmacs/delete-current-buffer-file
    "E"  #'petmacs/sudo-edit
    "i"  #'insert-file
    "l"  #'find-file-literally
	"w"  #'find-file-other-window
    "F"  #'find-file-other-frame
    "j"  #'dired-jump
    "t"  #'treemacs
    "T"  #'treemacs-find-file
    "B"  #'treemacs-bookmark
    "M-t" #'treemacs-find-tag  ;; Focus tag in file tree
    "L"  #'consult-locate
    "r"  #'consult-recent-file
    "R"  #'petmacs/rename-current-buffer-file
	"s"  #'save-buffer
    "S"  #'evil-write-all
    "b"  #'consult-bookmark
    "B"  #'treemacs-bookmark

    "m" #'consult-mark
    "M" #'consult-global-mark

	;; "fy" prefix
    "yy" #'petmacs/copy-file-path
    "yY" #'petmacs/projectile-copy-file-path
	"yd" #'petmacs/copy-directory-path
    "yD" #'petmacs/projectile-copy-directory-path
    "yn" #'petmacs/copy-file-name
    "yN" #'petmacs/copy-file-name-base
    "yb" #'petmacs/copy-buffer-name

    ;; "fv" prefix
    "vd" #'add-dir-local-variable
    "vf" #'add-file-local-variable
    "vp" #'add-file-local-variable-prop-line

	;; "fC" prefix

    "Cd" #'petmacs/unix2dos
    "Cr" #'save-buffer-gbk-as-utf8

	;; "fe" prefix
    "es" #'petmacs/goto-org-global-schedules
    "et" #'petmacs/goto-org-global-job-gtds
    "ed" #'petmacs/find-dotfile
    "eD" #'petmacs/find-user-early-init-file
    "er" #'petmacs/reload-init-file
    ))


(leader-declare-prefix
  "h" "help")
(leader-with-prefix "h"
  (leader-set-keys
    "b" #'describe-bindings
    "c" #'describe-char
    "f" #'describe-function
    "k" #'describe-key
    "K" #'describe-keymap
    "p" #'describe-package
    "t" #'describe-text-properties
    "T" #'describe-theme
    "v" #'describe-variable))

(leader-declare-prefix
  "b" "buffer"
  "bN" "new buffer")
(leader-with-prefix "b"
  (leader-set-keys
    "1" #'buffer-to-window-1
    "2" #'buffer-to-window-2
    "3" #'buffer-to-window-3
    "4" #'buffer-to-window-4
    "5" #'buffer-to-window-5
    "6" #'buffer-to-window-6
    "7" #'buffer-to-window-7
    "8" #'buffer-to-window-8
    "9" #'buffer-to-window-9
    "C-d" #'petmacs/kill-other-buffers
    "d" #'petmacs/kill-this-buffer
	;; "d" #'kill-this-buffer

    "f" #'apheleia-format-buffer

    ;; new buffer
    "Nf" #'petmacs/new-empty-buffer-new-frame
    "Nh" #'petmacs/new-empty-buffer-left
    "Nj" #'petmacs/new-empty-buffer-below
    "Nk" #'petmacs/new-empty-buffer-above
    "Nl" #'petmacs/new-empty-buffer-right
    "Nn" #'petmacs/new-empty-buffer

    "b" #'consult-buffer
    "B" #'consult-buffer-other-window
    "i" #'petmacs/imenu-list-smart-toggle
    "I" #'ibuffer
    "w" #'read-only-mode
    "n" #'next-buffer
    "p" #'previous-buffer
    "R" #'petmacs/revert-this-buffer
    "s" #'petmacs/goto-scratch-buffer
    "x" #'kill-buffer-and-window
    "h" #'petmacs/goto-dashboard
    "m"  #'petmacs/toggle-maximize-buffer
    "Y" #'petmacs/copy-whole-buffer-to-clipboard
    "j" #'ace-window
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
  "w" "window"
  "wc" "writeroom")
(leader-with-prefix "w"
  (leader-set-keys
    "."  #'ace-window-hydra/body
    "j"  #'ace-window
    "r"  #'winner-undo
    "b"  #'petmacs/switch-to-minibuffer-window
    "d"  #'petmacs/delete-window
    "D"  #'ace-delete-window
    "F"  #'make-frame

    "pt"  #'popper-toggle-type
    "pp"  #'popper-toggle
    "pc"  #'popper-cycle

    "o"  #'other-frame
    "h"  #'evil-window-left
    "H"  #'evil-window-move-far-left
    "j"  #'evil-window-down
    "J"  #'evil-window-move-very-bottom
    "k"  #'evil-window-up
    "l"  #'evil-window-right
    "K"  #'evil-window-move-very-top
    "l"  #'evil-window-right
    "L"  #'evil-window-move-far-right
    "m" #'petmacs/switch-to-minibuffer-window

    "v"  #'split-window-right
    "V"  #'split-window-right-and-focus
    "w"  #'other-window
    "x"  #'kill-buffer-and-window
    "/"  #'split-window-right
    "U"  #'winner-redo
    "u"  #'winner-undo
    "-"  #'split-window-below
    "s"  #'split-window-below
    "S"  #'split-window-below-and-focus

    ;; "wc"
    "cc"  #'writeroom-mode
    "c." #'writeroom-mode-hydra/body
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
  "t" "toggles"
  "tt" "theme"
  "tp" "proxy"
  )
(leader-with-prefix "t"
  (leader-set-keys
    "-" #'centered-cursor-mode
    "'" #'petmacs/open-gnome-terminal

    "o" #'toggle-one-window

    "cp" #'corfu-popupinfo-toggle
    "cs" #'prettify-symbols-mode

    "l" #'display-fill-column-indicator-mode
    "n" #'display-line-numbers-mode
    "f" #'focus-mode
    "F" #'toggle-frame-fullscreen
    "x" #'read-only-mode
    "M" #'maximize-window

    "h" #'global-hl-line-mode

    "z" #'writeroom-mode

    "tt" #'modus-themes-toggle
    "ts" #'petmacs/consult-theme

    ;; "tp" prefix
    "pp" #'toggle-http-proxy
    ))

(if (equal petmacs-checker 'flycheck)
    (leader-with-prefix "t"
      (leader-set-keys
        "s" #'flycheck-mode
        ))
  (leader-with-prefix "t"
    (leader-set-keys
      "s" #'flymake-mode
      ))
  )

(leader-declare-prefix
  "F" "frame")
(leader-with-prefix "F"
  (leader-set-keys
    ;; "f" #'find-file-other-frame
    "."  #'transwin-hydra/body
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
    "w" 'evil-avy-goto-word-or-subword-1
    "d" #'consult-dir
    "D" #'dired-jump-other-window
    "c" #'goto-last-change
    "j" 'evil-avy-goto-char
    "J" #'evil-avy-goto-char-2
    "l" 'evil-avy-goto-line
    ;; "t" #'magit-todos-list
    "t" #'hl-todo-rg-project
    "y" #'consult-yank-pop
    ))

(leader-declare-prefix
  "e" "error")
(if (equal petmacs-checker 'flycheck)
    (leader-with-prefix "e"
      (leader-set-keys
        ;; "l" #'flycheck-list-errors
        "l" #'consult-flycheck
        "c" #'flycheck-clear
        "h" #'flycheck-describe-checker
        "n" #'petmacs/next-error
        "N" #'petmacs/previous-error
        "p" #'petmacs/previous-error
        "s" #'flycheck-select-checker
        "S" #'flycheck-set-checker-executable
        "v" #'flycheck-verify-setup
        "y" #'flycheck-copy-errors-as-kill
        "x" #'flycheck-explain-error-at-point))
  (leader-with-prefix "e"
    (leader-set-keys
      "l" #'consult-flymake
      "n" #'flymake-goto-next-error
      "p" #'flymake-goto-prev-error
      "b" #'flymake-show-buffer-diagnostics
      "a" #'flymake-show-project-diagnostics)))


(leader-declare-prefix
  "D" "Diff/Compare"
  "Db" "Buffers"
  "Dd" "Directories"
  "Df" "files"
  "Dm" "merge"
  "Dr" "region"
  "Dw" "window"
  "Dmb" "merge buffers"
  "Dmd" "merge directories"
  "Dmf" "merge files"
  "Dmr" "merge revisions"
  )
(leader-with-prefix "D"
  (leader-set-keys
    "s" #'ediff-show-registry ;; "Show registry"
    "h" #'ediff-documentation ;; "Documentation"
    ;; "b"  "Buffers"
    "b3" #'ediff-buffers3   ;; "Between 3 buffers..."
    "bb" #'ediff-buffers    ;; "Between 2 buffers..."
    "bB" #'ediff-backup     ;; With backup file...
    "bp" #'ediff-patch-buffer ;; "With a patch..."
    ;; "d" "Directories"
    "d3" #'ediff-directories3 ;;  "Between 3 directories..."
    "dd" #'ediff-directories ;; "Between 2 directories..."
    "dr" #'ediff-directory-revisions ;; "Using SCM revisions..."
    ;; "f" "Files"
    "f3" #'ediff-files3 ;; "Between 3 files..."
    "ff" #'ediff-files  ;; "Between 2 files..."
    "fp" #'ediff-patch-file ;; "With a patch..."
    "fv" #'ediff-revision ;; "Between file revisions..."
    ;; "mb" "Merge Buffers"
    "mb3" #'ediff-merge-buffers-with-ancestor ;; "3-way merge..."
    "mbb" #'ediff-merge-buffers ;; "2-way merge..."
    ;; "md" "merge Directories"
    "md3" #'ediff-merge-directories-with-ancestor ;; "3-way merge..."
    "mdd" #'ediff-merge-directories ;; "2-way merge..."
    ;; "mf" "merge Files"
    "mf3" #'ediff-merge-files-with-ancestor ;;"3-way merge..."
    "mff" #'ediff-merge-files ;; "2-way merge..."
    ;; "mr" "merge Revisions"
    "mr3" #'ediff-merge-revisions-with-ancestor ;; "3-way merge..."
    "mrr" #'ediff-merge-revisions ;; "2-way merge..."
    ;; "r" "Regions"
    "rl" #'ediff-regions-linewise ;; "Between 2 regions (linewise)..."
    "rw" #'ediff-regions-wordwise ;; "Between 2 regions (wordwise)..."
    ;; "w" "Windows"
    "wl" #'ediff-windows-linewise ;;"Linewise between visible text..."
    "ww" #'ediff-windows-wordwise ;; "Wordwise between visible text..."
    ))

(leader-declare-prefix
  "p" "project")
(leader-with-prefix "p"
  (leader-set-keys
    "!" 'projectile-run-shell-command-in-root
    "."  #'consult-project-extra-find
    ;; "'"  #'petmacs/projectile-shell-pop
    "&" 'projectile-run-async-shell-command-in-root
    "%" 'projectile-replace-regexp
    "a" 'projectile-toggle-between-implementation-and-test
    "u" 'projectile-run-project
    "D" 'projectile-dired
    "e" 'projectile-edit-dir-locals
    "F" 'projectile-find-file-dwim
    "E" 'projectile-find-references
    "g" 'projectile-find-tag
    "G" 'projectile-regenerate-tags
    "i" 'projectile-install-project
    "I" 'projectile-invalidate-cache
    "k" 'projectile-kill-buffers
    "R" 'projectile-replace
    "T" 'projectile-test-project
    "v" 'projectile-vc

    "'"  #'multi-vterm-project
    "t"  #'petmacs/treemacs-project-toggle
    ;; "t" #'petmacs/toggle-treemacs-and-symbols-outline
    "b"  #'consult-projectile-switch-to-buffer
    "d"  #'consult-projectile-find-dir
    "p"  #'consult-projectile-switch-project
    "f"  #'consult-projectile-find-file
    ;; "f"  #'affe-find
    "r"  #'consult-projectile-recentf
    "o"  #'org-projectile/goto-project-todos
    "c"  #'org-projectile-project-todo-completing-read
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

(cond ((and (equal petmacs-dap-mode-impl 'dape) emacs/>=29p)
       (leader-declare-prefix
         "d" "debug"
         "db"  "breakpoints"
         "dd"  "debugging"
         "dS"  "switch")
       (leader-with-prefix "d"
         (leader-set-keys
           "." #'dape-hydra/body

           "q"  #'dape-quit
           "D"  #'dape-disconnect-quit

           ;; breakpoints
           "bb" #'dape-breakpoint-toggle
           "bc" #'dape-breakpoint-expression
           "bl" #'dape-breakpoint-log
           "bD" #'dape-breakpoint-remove-all
           ;; debuging/running
           "dd" #'dape

           ;; switching
           "Sw" #'dape-watch-dwim
           "Sm" #'dape-read-memory
           "St" #'dape-select-thread
           "Ss" #'dape-select-stack
           "Ss" #'dape-info
           "Sr"  #'dape-repl

           ;; stepping
           "n"  #'dape-next
           "s"  #'dape-next
           "o"  #'dape-step-out
           "c"  #'dape-continue
           "i"  #'dape-step-in
           "r"  #'dape-restart
           "p"  #'dape-pause
           "k"  #'dape-kill
           "r"  #'dape-restart
           )))
      (t
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
           "q"  #'dap-disconnect
           "Q"  #'dap-delete-all-sessions
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
           "n"  #'dap-next
           "v"  #'dap-ui-inspect-thing-at-point
           ;; switching
           "Ss" #'dap-switch-session
           "St" #'dap-switch-thread
           "Sf" #'dap-switch-frame
           ;; windows
           "wo" #'dap-go-to-output-buffer
           "wl" #'dap-ui-locals
           "ws" #'dap-ui-sessions
           "wb" #'dap-ui-breakpoints))))


;;; major mode keybinidngs ;;;;;;;;;;;;;;;;;;

;;; lsp for major mode

(cond ((equal petmacs-lsp-mode-impl 'lsp-bridge-mode)
       (dolist (mode '(c-mode c++-mode cmake-mode java-mode python-mode ruby-mode lua-mode rust-mode rustic-mode erlang-mode elixir-mode go-mode haskell-mode haskell-literate-mode dart-mode scala-mode typescript-mode typescript-tsx-mode js2-mode js-mode rjsx-mode tuareg-mode latex-mode Tex-latex-mode texmode context-mode texinfo-mode bibtex-mode clojure-mode clojurec-mode clojurescript-mode clojurex-mode sh-mode web-mode css-mode elm-mode emacs-lisp-mode ielm-mode lisp-interaction-mode org-mode php-mode yaml-mode zig-mode groovy-mode dockerfile-mode d-mode f90-mode fortran-mode nix-mode ess-r-mode verilog-mode))
         (leader-declare-prefix-for-major-mode mode
           "=" "format"
           "a" "actions"
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
      ((equal petmacs-lsp-mode-impl 'eglot)

       (dolist (mode petmacs-lsp-active-modes)
         (leader-declare-prefix-for-major-mode mode
           "=" "format"
           "a" "actions"
           "g" "goto"
           "G" "goto (other window)"
           ;; "b" "backends"
           ;; "p" "peek"
           ;; "pR" "peek reference"
           ;; "T" "toggle module"
           "r" "refactor"
           "h" "help"
           ;; "F" "folders"
           ;; "x" "text/code"
           )

         (leader-set-keys-for-major-mode mode
           ;; format
           "=o" #'eglot-code-action-organize-imports

           ;; help
           "hh" #'eldoc-doc-buffer

           ;; actions
           "aa" #'eglot-code-actions

           ;; rename
           "rr" #'eglot-rename

           ;; goto
           "gd" #'xref-goto-xref
           "gr" #'xref-find-references
           "gD" #'eglot-find-declaration
           "gi" #'eglot-find-implementation
           "gt" #'eglot-find-typeDefinition
           "gb" #'xref-pop-marker-stack
           "gF" #'xref-find-definitions-other-frame
           "gs" #'consult-eglot-symbols

           ;; goto other window
           "Gd" #'xref-find-definitions-other-window

           "gT" #'eglot-hierarchy-type-hierarchy
           "gC" #'eglot-hierarchy-call-hierarchy
           )

         ))
      (t
       (dolist (mode petmacs-lsp-active-modes)
         (leader-declare-prefix-for-major-mode mode
           "=" "format"
           "a" "actions"
           "g" "goto"
           "G" "goto (other window)"
           "p" "peek"
           "pR" "peek reference"
           "b" "backends"
           "T" "toggle module"
           "r" "refactor"
           "h" "help"
           "F" "folders"
           "x" "text/code")
         (leader-set-keys-for-major-mode mode
           ;; format
           "=b" #'lsp-format-buffer
           "=r" #'lsp-format-region
           "=o" #'lsp-organize-imports

           ;; code actions
           "aa" #'lsp-execute-code-action
           "ah" #'lsp-document-highlight
           "al" #'lsp-avy-lens

           ;; format
           "rr" #'lsp-rename

           ;; goto
           "gd" #'lsp-find-definition
           "gD" #'lsp-find-declaration
           "ge" #'lsp-treemacs-errors-list
           "gC" #'lsp-treemacs-call-hierarchy
           "gT" #'lsp-treemacs-type-hierarchy
           "gr" #'lsp-find-references
           "gi" #'lsp-find-implementation
           "gt" #'lsp-find-type-definition

           "gs" #'petmacs/consult-lsp-file-symbols
           "gS" #'consult-lsp-symbols
           "gm" #'symbols-outline-show

           "gb" #'xref-pop-marker-stack
           "gF" #'xref-find-definitions-other-frame

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

           ;; backends
           "bd" #'lsp-describe-session
           "br" #'lsp-workspace-restart
           "bs" #'lsp-workspace-shutdown


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
           "FR" #'lsp-workspace-folders-remove
           "Fb" #'lsp-workspace-blacklist-remove


           ;; toggles
           "TD" #'lsp-modeline-diagnostics-mode
           "Tl" #'lsp-lens-mode
           "Td" #'lsp-ui-doc-mode
           "Ts" #'lsp-toggle-signature-auto-activate
           "TS" #'lsp-ui-sideline-mode))

       (dolist (mode '(java-mode java-ts-mode))
         (leader-declare-prefix-for-major-mode mode
           "mc"  "compile/create"
           "mgk"  "type hierarchy"
           "mra"  "add/assign"
           "mrc"  "create/convert"
           "mrg"  "generate"
           "mre"  "extract"
           "mt"  "test")

         (leader-set-keys-for-major-mode mode
           "wu"  'lsp-java-update-project-configuration

           ;; refactoring
           "ro" 'lsp-java-organize-imports
           "rcp" 'lsp-java-create-parameter
           "rcf" 'lsp-java-create-field
           "rci" 'lsp-java-convert-to-static-import
           "rec" 'lsp-java-extract-to-constant
           "rel" 'lsp-java-extract-to-local-variable
           "rem" 'lsp-java-extract-method

           ;; assign/add
           "rai" 'lsp-java-add-import
           "ram" 'lsp-java-add-unimplemented-methods
           "rat" 'lsp-java-add-throws
           "raa" 'lsp-java-assign-all
           "raf" 'lsp-java-assign-to-field
           "raF" 'lsp-java-assign-statement-to-field
           "ral" 'lsp-java-assign-statement-to-local

           ;; generate
           "rgt" 'lsp-java-generate-to-string
           "rge" 'lsp-java-generate-equals-and-hash-code
           "rgo" 'lsp-java-generate-overrides
           "rgg" 'lsp-java-generate-getters-and-setters

           ;; create/compile
           "cc"  'lsp-java-build-project
           "cp"  'lsp-java-spring-initializr

           "gkk" 'lsp-java-type-hierarchy
           "gku" 'petmacs/lsp-java-super-type
           "gks" 'petmacs/lsp-java-sub-type

           ;; test
           "tb" 'lsp-jt-browser
           ))))

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

    "ri" #'python-fix-imports
    "rI" #'petmacs/python-remove-unused-imports
    "rd" #'sphinx-doc

    "va" #'pyvenv-activate
    "vd" #'pyvenv-deactivate
    "vw" #'pyvenv-workon
    ))

;;; java mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dolist (mode '(java-mode java-ts-mode))
  (leader-declare-prefix-for-major-mode mode
    "mm" "maven"
    "mmg" "goto"
    "mmt" "tests")

  (leader-set-keys-for-major-mode mode
    "mga"    #'maven-test-toggle-between-test-and-class
    "mgA"    #'maven-test-toggle-between-test-and-class-other-window
    "mta"    #'maven-test-all
    "mt C-a" #'maven-test-clean-test-all
    "mtb"    #'maven-test-file
    "mti"    #'maven-test-install
    "mtt"    #'maven-test-method))

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

(leader-declare-prefix-for-major-mode 'org-mode
  "mb"  "babel"
  "mC"  "org-clocks"
  "md"  "dates"
  "me"  "export"
  "mf"  "feeds"
  "mi"  "insert"
  "miD" "download"
  "mm"  "more"
  "ms"  "trees/subtrees"
  "mT"  "toggles"
  "mt"  "tables"
  "mtd" "delete"
  "mti" "insert"
  "mtt" "toggle"
  "mx"  "text")

(leader-set-keys-for-major-mode 'org-mode
  "'" 'org-edit-special
  "c" 'org-capture

  ;; Clock
  ;; These keybindings should match those under the "aoC" prefix (below)
  "Cc" 'org-clock-cancel
  "Cd" 'org-clock-display
  "Ce" 'org-evaluate-time-range
  "Cg" 'org-clock-goto
  "Ci" 'org-clock-in
  "CI" 'org-clock-in-last
  "Co" 'org-clock-out
  "CR" 'org-clock-report
  "Cr" 'org-resolve-clocks

  "dd" 'org-deadline
  "ds" 'org-schedule
  "dt" 'org-time-stamp
  "dT" 'org-time-stamp-inactive
  "ee" 'org-export-dispatch
  "fi" 'org-feed-goto-inbox
  "fu" 'org-feed-update-all

  "a" 'org-agenda
  "[" 'org-agenda-file-to-front
  "]" 'org-remove-file

  "p" 'org-priority

  "Tc" 'org-toggle-checkbox
  "Te" 'org-toggle-pretty-entities
  "Ti" 'org-toggle-inline-images
  "Tn" 'org-num-mode
  "Tl" 'org-toggle-link-display
  "Tt" 'org-show-todo-tree
  "TT" 'org-todo
  "TV" 'space-doc-mode
  "Tx" 'org-latex-preview

  ;; More cycling options (timestamps, headlines, items, properties)
  "L" 'org-shiftright
  "H" 'org-shiftleft
  "J" 'org-shiftdown
  "K" 'org-shiftup

  ;; Change between TODO sets
  "C-S-l" 'org-shiftcontrolright
  "C-S-h" 'org-shiftcontrolleft
  "C-S-j" 'org-shiftcontroldown
  "C-S-k" 'org-shiftcontrolup

  ;; Subtree editing
  "sa" 'org-toggle-archive-tag
  "sA" 'org-archive-subtree-default
  "sb" 'org-tree-to-indirect-buffer
  "sd" 'org-cut-subtree
  "sy" 'org-copy-subtree
  "sp" 'org-paste-subtree
  "sh" 'org-promote-subtree
  "sj" 'org-move-subtree-down
  "sk" 'org-move-subtree-up
  "sl" 'org-demote-subtree
  "sn" 'org-narrow-to-subtree
  "sw" 'widen
  "sr" 'org-refile
  "ss" 'org-sparse-tree
  "sS" 'org-sort

  ;; tables
  "ta" 'org-table-align
  "tb" 'org-table-blank-field
  "tc" 'org-table-convert
  "tdc" 'org-table-delete-column
  "tdr" 'org-table-kill-row
  "te" 'org-table-eval-formula
  "tE" 'org-table-export
  "tf" 'org-table-field-info
  "th" 'org-table-previous-field
  "tH" 'org-table-move-column-left
  "tic" 'org-table-insert-column
  "tih" 'org-table-insert-hline
  "tiH" 'org-table-hline-and-move
  "tir" 'org-table-insert-row
  "tI" 'org-table-import
  "tj" 'org-table-next-row
  "tJ" 'org-table-move-row-down
  "tK" 'org-table-move-row-up
  "tl" 'org-table-next-field
  "tL" 'org-table-move-column-right
  "tn" 'org-table-create
  "tN" 'org-table-create-with-table.el
  "tr" 'org-table-recalculate
  "tR" 'org-table-recalculate-buffer-tables
  "ts" 'org-table-sort-lines
  "ttf" 'org-table-toggle-formula-debugger
  "tto" 'org-table-toggle-coordinate-overlays
  "tw" 'org-table-wrap-region

  ;; Source blocks / org-babel
  "bp"     'org-babel-previous-src-block
  "bn"     'org-babel-next-src-block
  "be"     'org-babel-execute-maybe
  "bo"     'org-babel-open-src-block-result
  "bv"     'org-babel-expand-src-block
  "bu"     'org-babel-goto-src-block-head
  "bg"     'org-babel-goto-named-src-block
  "br"     'org-babel-goto-named-result
  "bb"     'org-babel-execute-buffer
  "bs"     'org-babel-execute-subtree
  "bd"     'org-babel-demarcate-block
  "bt"     'org-babel-tangle
  "bf"     'org-babel-tangle-file
  "bc"     'org-babel-check-src-block
  "bj"     'org-babel-insert-header-arg
  "bl"     'org-babel-load-in-session
  "bi"     'org-babel-lob-ingest
  "bI"     'org-babel-view-src-block-info
  "bz"     'org-babel-switch-to-session
  "bZ"     'org-babel-switch-to-session-with-code
  "ba"     'org-babel-sha1-hash
  "bx"     'org-babel-do-key-sequence-in-edit-buffer

  "*" 'org-ctrl-c-star
  "-" 'org-ctrl-c-minus
  "#" 'org-update-statistics-cookies
  "RET"   'org-ctrl-c-ret
  "M-RET" 'org-meta-return
  ;; attachments
  "A" 'org-attach
  ;; insertion
  "ib" 'org-insert-structure-template
  "id" 'org-insert-drawer
  "ie" 'org-set-effort
  "if" 'org-footnote-new
  "ih" 'org-insert-heading
  "iH" 'org-insert-heading-after-current
  "ii" 'org-insert-item
  "il" 'org-insert-link
  "in" 'org-add-note
  "ip" 'org-set-property
  "is" 'org-insert-subheading
  "it" 'org-set-tags-command
  )

(provide 'init-keybindings)
