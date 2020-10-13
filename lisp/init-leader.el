;; init-leader.el --- Setup c/c++ IDE.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; install fcitx fcitx-googlepinyin
;; create ~/.xprofile which has:
;; ```sh
;; export GTK_IM_MODULE=fcitx
;; export QT_IM_MODULE=fcitx
;; export XMODIFIERS=@im=fcitx
;; fcitx &
;; ```
;; write `LC_CTYPE="zh_CN.UTF-8"` to /etc/default/locale & reboot
(global-set-key (kbd "C-SPC") 'nil) ;; switch to chinese
(global-set-key (kbd "C-M-k") 'company-files)

(setq leader-nnorm-key petmacs-evil-major-leader-insert-default-key
      leader-key petmacs-evil-leader-key
      leader-major-mode-key petmacs-evil-major-leader-key)

(use-package spaceleader
  :ensure nil
  :commands (
	     leader-map
	     leader/with-prefix
	     leader/with-major-mode-prefix
	     leader/set-keys
	     leader/set-keys-for-mode
	     leader/set-keys-for-mode!
	     leader/set-keys-for-major-mode))

(leader/set-keys
  "'"   #'petmacs/pop-eshell
  "?"   #'counsel-descbinds
  "/"   #'counsel-projectile-rg
  "v"   #'er/expand-region
  "u"   #'universal-argument
  "d"   #'xref-pop-marker-stack
  "TAB"  #'petmacs/alternate-buffer)

(leader/set-keys
  "a" "applications"
  "ad"  #'deer
  "ap"  #'list-processes
  "aP"  #'proced
  "ar"  #'ranger
  "ak"  #'paradox-list-packages
  "au"  #'paradox-upgrade-packages
  "aw"  #'whitespace-cleanup

  "ao" "org"
  "aoa" #'org-agenda-list
  "aoc" #'org-capture
  "aoe" #'org-store-agenda-views
  "aop" #'org-projectile/capture
  "aoo" #'org-agenda
  "aol" #'org-store-link
  "aom" #'org-tags-view
  "aos" #'org-search-view
  "aot" #'org-todo-list
  "ao/" #'org-occur-in-agenda-files
  )

(leader/set-keys
  "f" "files"
  "ff"  #'counsel-find-file
  "fw"  #'find-file-other-window
  "fF"  #'find-file-other-frame
  "fj"  #'dired-jump
  "ft"  #'treemacs
  "fB"  #'treemacs-bookmark
  "fT"  #'treemacs-find-file
  "fL"  #'counsel-locate
  "fr"  #'counsel-recentf
  "fR"  #'petmacs/rename-current-buffer-file
  "fs"  #'save-buffer
  "fS"  #'evil-write-all
  "fc"  #'petmacs/copy-file
  "fb"  #'counsel-bookmark
  "fB"  #'treemacs-bookmark

  "fy" "copy"
  "fyy" #'petmacs/copy-file-path
  "fyY" #'petmacs/projectile-copy-file-path
  "fyd" #'petmacs/copy-directory-path
  "fyn" #'petmacs/copy-file-name

  "fv" "variable"
  "fvd" #'add-dir-local-variable
  "fvf" #'add-file-local-variable
  "fvp" #'add-file-local-variable-prop-line

  "fe" "emacs conf"
  "feo" #'petmacs/find-org-global-todos
  "fec" #'petmacs/find-custom-file
  "fed" #'petmacs/find-dotfile

  "fC" "unix <-> dos"
  "fCu" #'dos2unix
  "fCd" #'unix2dos
  "fCr" #'petmacs/save-buffer-gbk-as-utf8
  )

(leader/set-keys
  "t" "toggle"
  "t-" #'centered-cursor-mode
  "ts" #'flycheck-mode
  "tf" #'focus-mode
  "tF" #'toggle-frame-fullscreen
  "tM" #'maximize-window

  "tp" "proxy"
  "tpt" #'proxy-http-toggle
  "tpd" #'proxy-http-disable
  "tpc" #'proxy-http-customize
  "tps" #'proxy-http-show
  )

(leader/set-keys
  "F" "Frame"
  "Ff" #'find-file-other-frame
  "Fd" #'delete-frame
  "FD" #'delete-other-frames
  "Fb" #'switch-to-buffer-other-frame
  "FB" #'display-buffer-other-frame
  "Fo" #'other-frame
  "FO" #'dired-other-frame
  "Fn" #'make-frame)

(leader/set-keys
  "b" "buffer"
  "bb" #'ivy-switch-buffer
  "bB" #'ibuffer
  "bd" #'kill-this-buffer
  "bn" #'next-buffer
  "bp" #'previous-buffer
  "bR" #'petmacs/revert-this-buffer
  "bs" #'petmacs/goto-scratch-buffer
  "bx" #'kill-buffer-and-window
  "bh" #'petmacs/goto-dashboard
  "bm" #'petmacs/switch-to-minibuffer-window
  "bY" #'petmacs/copy-whole-buffer-to-clipboard
  "ba" #'persp-add-buffer
  "br" #'persp-remove-buffer
  "bj" #'ace-window
  "bt" #'imenu-list-smart-toggle
  "bI" #'lsp-ui-imenu
  "bi" #'imenu-list)

(leader/set-keys
  "B" "bookmarks"
  "Bs" #'bookmark-set
  "Bd" #'bookmark-delete
  "Br" #'bookmark-rename
  "Bl" #'bookmark-bmenu-list)

(leader/set-keys
  "n" "narrow"
  "nf" #'narrow-to-defun
  "nr" #'narrow-to-region
  "np" #'narrow-to-page
  "nw" #'widen)

(leader/set-keys
  "w" "window"
  "w."  #'hydra-frame-window/body
  "wc"  #'olivetti-mode
  "wd"  #'delete-window
  "wD"  #'ace-delete-window

  "wp" "popout window"
  "wpm" #'petmacs/shackle-popup-message-buffer
  "wpc" #'petmacs/shackle-popup-compilation-buffer)

(leader/set-keys
  "g" "git"
  "gc"  #'magit-clone
  "gs"  #'magit-status
  "gi"  #'magit-init
  "gl"  #'magit-log-head
  "gL"  #'magit-list-repositories
  "gm"  #'magit-dispatch
  "gS"  #'magit-stage-file
  "gU"  #'magit-unstage-file
  "gw"  #'browse-at-remote

  "gf" "magit file"
  "gff" #'magit-find-file
  "gfl" #'magit-log-buffer-file
  "gfd" #'magit-diff)

(leader/set-keys
  "j" "jump"
  "ji" #'petmacs/counsel-jump-in-buffer
  "jw" #'evil-avy-goto-word-or-subword-1
  "jD" #'deer-jump-other-window
  "jc" #'goto-last-change
  "jd" #'deer
  "jj" #'avy-goto-char-timer
  "jJ" #'avy-goto-char-2
  )

(leader/set-keys
  "e" "error"
  "eb" #'flycheck-buffer
  "ec" #'flycheck-clear
  "eh" #'flycheck-describe-checker
  "el" #'petmacs/toggle-flycheck-error-list
  "en" #'petmacs/next-error
  "eN" #'petmacs/previous-error
  "ep" #'petmacs/previous-error
  "es" #'flycheck-select-checker
  "eS" #'flycheck-set-checker-executable
  "ev" #'flycheck-verify-setup
  "ey" #'flycheck-copy-errors-as-kill
  "ex" #'flycheck-explain-error-at-point)

(leader/set-keys
  "p" "project"
  "p'"  #'petmacs/projectile-pop-eshell
  "pt"  #'petmacs/treemacs-project-toggle
  "pb"  #'counsel-projectile-switch-to-buffer
  "pd"  #'counsel-projectile-find-dir
  "pp"  #'counsel-projectile-switch-project
  "pf"  #'counsel-projectile-find-file
  "pr"  #'projectile-recentf
  "po"  #'org-projectile/goto-todos
  "pl"  #'petmacs/ivy-persp-switch-project
  "pv"  #'projectile-vc)

(leader/set-keys
  "i" "insert"
  "is" #'ivy-yasnippet
  "if" #'insert-file
  "iu" #'counsel-unicode-char)



(leader/set-keys
  "T" "Theme"
  "Ts"  #'petmacs/select-theme
  "Tn"  #'petmacs/cycle-theme)

(leader/set-keys
  "q" "quit"
  "qq" #'petmacs/frame-killer
  "qQ" #'kill-emacs
  "qh" #'suspend-frame
  "qR" #'restart-emacs)

(leader/set-keys
  "o" "origami (code fold)"
  "o."  #'origami-hydra/body)

;;; major mode keybinidngs

;;; lsp major mode settings
(dolist (mode petmacs-lsp-active-modes)
  (leader/set-keys-for-major-mode mode
    "r" "refactor"
    "rb" #'lsp-format-buffer
    "rr" #'lsp-format-region
    "rR" #'lsp-rename

    "g" "goto"
    "gr" #'lsp-find-references
    "gt" #'lsp-find-type-definition
    "gd" #'xref-find-definitions
    "gD" #'lsp-find-declaration
    "gf" #'xref-find-definitions-other-frame
    "gi" #'lsp-find-implementation
    "gs" #'lsp-ui-find-workspace-symbol
    "gM" #'lsp-ui-imenu

    "G" "goto (other window)"

    "Gr" #'petmacs/lsp-find-references-other-window
    "Gt" #'petmacs/lsp-find-type-definition-other-window
    "Gd" #'xref-find-definitions-other-window
    "GD" #'petmacs/lsp-find-declaration-other-window
    "Gi" #'petmacs/lsp-find-implementation-other-window

    "p" "peek"
    "pd" #'lsp-ui-peek-find-definitions
    "pi" #'lsp-ui-peek-find-implementation
    "pr" #'lsp-ui-peek-find-references
    "pRn" #'lsp-ui-find-next-reference
    "pRp" #'lsp-ui-find-prev-reference

    "b" "backend"
    "ba" #'lsp-execute-code-action
    "bd" #'lsp-describe-session
    "br" #'lsp-restart-workspace
    "bs" #'lsp-shutdown-workspace

    "h" "help"
    "hh" #'lsp-describe-thing-at-point


    "d" "debug"
    "d." #'dap-hydra
    ;; repl
    "d'"  #'dap-ui-repl
    ;; abandon
    "da"  #'dap-disconnect
    "dA"  #'dap-delete-all-sessions

    "db"  "breakpoints"
    "dbb" #'dap-breakpoint-toggle
    "dbc" #'dap-breakpoint-condition
    "dbl" #'dap-breakpoint-log-message
    "dbh" #'dap-breakpoint-hit-condition
    "dba" #'dap-breakpoint-add
    "dbd" #'dap-breakpoint-delete
    "dbD" #'dap-breakpoint-delete-all

    "dd"  "debugging"
    "ddd" #'dap-debug
    "dde" #'dap-debug-edit-template
    "ddl" #'dap-debug-last
    "ddr" #'dap-debug-recent

    "de"  "eval"
    "dee" #'dap-eval
    "der" #'dap-eval-region
    "det" #'dap-eval-thing-at-point
    "det" #'dap-ui-expressions-add

    "dI"  "inspect"
    "dIi" #'dap-ui-inspect
    "dIr" #'dap-ui-inspect-region
    "dIt" #'dap-ui-inspect-thing-at-point
    ;; stepping
    "dc"  #'dap-continue
    "di"  #'dap-step-in
    "do"  #'dap-step-out
    "dr"  #'dap-restart-frame
    "ds"  #'dap-next
    "dv"  #'dap-ui-inspect-thing-at-point

    "dS"  "switch"
    "dSs" #'dap-switch-session
    "dSt" #'dap-switch-thread
    "dSf" #'dap-switch-frame

    "dw"  "debug windows"
    "dwo" #'dap-go-to-output-buffer
    "dwl" #'dap-ui-locals
    "dws" #'dap-ui-sessions
    "dwb" #'dap-ui-breakpoints

    ;; toggles
    ;; "Td" #'lsp-ui-doc-mode
    ;; "Ts" #'lsp-ui-sideline-mode
    ;; "TF" #'petmacs/lsp-ui-doc-func
    ;; "TS" #'petmacs/lsp-ui-sideline-symb
    ;; "TI" #'petmacs/lsp-ui-sideline-ignore-duplicate
    ;; "Tl" #'lsp-lens-mode

    "F" "folders"
    "Fs" #'lsp-workspace-folders-switch
    "Fr" #'lsp-workspace-folders-remove
    "Fa" #'lsp-workspace-folders-add))


;;; python mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leader/set-keys-for-major-mode 'python-mode
  "c" "compile"
  "cc" #'petmacs/python-execute-file
  "cC" #'petmacs/python-execute-file-focus
  "ck" #'petmacs/quit-subjob

  "rI" #'py-isort-buffer
  "ri" #'petmacs/python-remove-unused-imports
  "rB" #'yapfify-buffer

  "sB" #'petmacs/python-shell-send-buffer-switch
  "sb" #'petmacs/python-shell-send-buffer
  "sb" #'petmacs/python-shell-send-buffer
  "sF" #'petmacs/python-shell-send-defun-switch
  "sf" #'petmacs/python-shell-send-defun
  "si" #'petmacs/python-start-or-switch-repl
  "sr" #'petmacs/python-shell-send-region
  "sR" #'petmacs/python-shell-send-region-switch
  "sk" #'petmacs/python-interrupt-repl
  "sq" #'petmacs/python-quit-repl

  "v" "virtual environment"
  "va" #'pyvenv-activate
  "vd" #'pyvenv-deactivate
  "vw" #'pyvenv-workon

  "vp" "pipenv"
  "vpa" #'pipenv-activate
  "vpd" #'pipenv-deactivate
  "vpi" #'pipenv-install
  "vpo" #'pipenv-open
  "vps" #'pipenv-shell
  "vpu" #'pipenv-uninstall)


;;; emacs lisp mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
  (leader/set-keys-for-major-mode mode
    "'"  #'ielm
    "si" #'ielm

    "c" "compile"
    "cc" #'emacs-lisp-byte-compile

    "e" "eval"
    "eb" #'eval-buffer
    "eC" #'petmacs/eval-current-form
    "ee" #'eval-last-sexp
    "er" #'eval-region
    "ef" #'eval-defun

    "gG" #'petmacs/nav-find-elisp-thing-at-point-other-window

    "t" "test"
    "tq" #'ert))

(provide 'init-leader)

;;; init-leader.el ends here
