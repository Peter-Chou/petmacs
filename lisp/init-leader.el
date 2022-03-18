;; init-leader.el --- Setup c/c++ IDE.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(define-key global-map (kbd "C-.") 'company-files)

;; (define-key global-map (kbd "C-l") 'evil-window-right)
;; (define-key global-map (kbd "C-h") 'evil-window-left)
;; (define-key global-map (kbd "C-k") 'evil-window-up)
;; (define-key global-map (kbd "C-j") 'evil-window-down)
;; (define-key global-map (kbd "C-S-l") 'evil-window-move-far-right)
;; (define-key global-map (kbd "C-S-h") 'evil-window-move-far-left)
;; (define-key global-map (kbd "C-S-k") 'evil-window-move-very-top)
;; (define-key global-map (kbd "C-S-j") 'evil-window-move-very-bottom)

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

(setq leader-nnorm-key petmacs-evil-major-leader-insert-default-key
      leader-key petmacs-evil-leader-key
      leader-major-mode-key petmacs-evil-major-leader-key)


(defalias 'which-key-declare-prefixes 'which-key-add-key-based-replacements)
(defalias 'which-key-declare-prefixes-for-mode
  'which-key-add-major-mode-key-based-replacements)

(use-package spaceleader
  :quelpa
  (spaceleader :fetcher github
  	       :repo "mohkale/spaceleader"
  	       :files ("*")))


(leader-declare-prefix
 "a"  '("apps" . "applications")
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
 "t" "toggle"
 "tp" "proxy"
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

 "d" "debug"
 "db"  "breakpoints"
 "dd"  "debugging"
 "de"  "eval"
 "dI"  "inspect"
 "dS"  "switch"
 "dw"  "debug windows"
 )

(leader-set-keys
 ;; "'"   #'petmacs/pop-eshell
 "'"   #'petmacs/shell-pop
 "?"   #'counsel-descbinds
 "/"   #'counsel-projectile-rg
 "v"   #'er/expand-region
 "u"   #'universal-argument
 ;; "d"   #'xref-pop-marker-stack
 "TAB"  #'petmacs/alternate-buffer)

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

(leader-with-prefix "f"
  (leader-set-keys

    "f"  #'counsel-find-file
    "w"  #'find-file-other-window
    "F"  #'find-file-other-frame
    "j"  #'dired-jump
    "t"  #'treemacs
    "B"  #'treemacs-bookmark
    "T"  #'treemacs-find-file
    "L"  #'counsel-locate
    "r"  #'counsel-recentf
    "R"  #'petmacs/rename-current-buffer-file
    "s"  #'save-buffer
    "S"  #'evil-write-all
    "c"  #'petmacs/copy-file
    "b"  #'counsel-bookmark
    "B"  #'treemacs-bookmark

    ;; "fy" prefix
    "yy" #'petmacs/copy-file-path
    "yY" #'petmacs/projectile-copy-file-path
    "yd" #'petmacs/copy-directory-path
    "yn" #'petmacs/copy-file-name

    ;; "fv" prefix
    "vd" #'add-dir-local-variable
    "vf" #'add-file-local-variable
    "vp" #'add-file-local-variable-prop-line

    ;; "fe" prefix
    "eo" #'petmacs/find-org-global-todos
    "ec" #'petmacs/find-custom-file
    "ed" #'petmacs/find-dotfile

    ;; "fC" prefix
    "Cu" #'dos2unix
    "Cd" #'unix2dos
    "Cr" #'petmacs/save-buffer-gbk-as-utf8
    ))

(leader-with-prefix "t"
  (leader-set-keys
    "-" #'centered-cursor-mode
    "'" #'petmacs/open-gnome-terminal
    "m" #'evil-visual-mark-mode
    "s" #'flycheck-mode
    "t" #'transwin-toggle-transparent-frame
    "l" #'display-fill-column-indicator-mode
    "n" #'display-line-numbers-mode
    "f" #'focus-mode
    "F" #'toggle-frame-fullscreen
    "x" #'read-only-mode
    "M" #'maximize-window

    ;; "tp" prefix
    "pt" #'proxy-http-toggle
    "pd" #'proxy-http-disable
    "pc" #'proxy-http-customize
    "ps" #'proxy-http-show
    ))

(leader-with-prefix "F"
  (leader-set-keys
    "f" #'find-file-other-frame
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


(leader-with-prefix "b"
  (leader-set-keys
    "b" #'ivy-switch-buffer
    "B" #'ibuffer
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
    "I" #'lsp-ui-imenu
    "i" #'imenu-list
    ))

(leader-with-prefix "B"
  (leader-set-keys
    "s" #'bookmark-set
    "d" #'bookmark-delete
    "r" #'bookmark-rename
    "l" #'bookmark-bmenu-list
    ))

(leader-with-prefix "n"
  (leader-set-keys
    "f" #'narrow-to-defun
    "r" #'narrow-to-region
    "p" #'narrow-to-page
    "w" #'widen
    ))

(leader-with-prefix "w"
  (leader-set-keys
    "."  #'hydra-frame-window/body
    "c"  #'olivetti-mode
    "r"  #'winner-undo
    "d"  #'delete-window
    "D"  #'ace-delete-window
    "1"  #'toggle-one-window

    ;; "wp" prefix
    "pm" #'petmacs/shackle-popup-message-buffer
    "pc" #'petmacs/shackle-popup-compilation-buffer
    ))

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
    "w"  #'browse-at-remote

  ;;; "gf" file
    "ff" #'magit-find-file
    "fl" #'magit-log-buffer-file
    "fd" #'magit-diff
    ))

(leader-with-prefix "j"
  (leader-set-keys
    "i" #'petmacs/counsel-jump-in-buffer
    "w" #'evil-avy-goto-word-or-subword-1
    "D" #'deer-jump-other-window
    "c" #'goto-last-change
    "d" #'deer
    "j" #'avy-goto-char-timer
    "J" #'avy-goto-char-2
    "t" #'ivy-magit-todos
    ))

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

(leader-with-prefix "p"
  (leader-set-keys
    ;; "'"  #'petmacs/projectile-pop-eshell
    "'"  #'petmacs/projectile-shell-pop
    ;;     (leader-set-keys "p'"   #'petmacs/projectile-pop-vterm)
    "t"  #'petmacs/treemacs-project-toggle
    "b"  #'counsel-projectile-switch-to-buffer
    "d"  #'counsel-projectile-find-dir
    "p"  #'counsel-projectile-switch-project
    "f"  #'counsel-projectile-find-file
    "r"  #'projectile-recentf
    "o"  #'org-projectile/goto-todos
    "l"  #'petmacs/ivy-persp-switch-project
    "v"  #'projectile-vc
    ))

(leader-with-prefix "i"
  (leader-set-keys
    "s" #'ivy-yasnippet
    "f" #'insert-file
    "u" #'counsel-unicode-char
    ))

(leader-with-prefix "T"
  (leader-set-keys
    "s"  #'petmacs/select-theme
    "n"  #'petmacs/cycle-theme
    ))

(leader-with-prefix "q"
  (leader-set-keys
    "q" #'petmacs/frame-killer
    "Q" #'kill-emacs
    "h" #'suspend-frame
    "R" #'restart-emacs
    ))

(leader-with-prefix "o"
  (leader-set-keys
    "o."  #'origami-hydra/body
    ))

;;; major mode keybinidngs ;;;;;;;;;;;;;;;;;;
(dolist (mode petmacs-lsp-active-modes)
  (leader-declare-prefix-for-major-mode mode
    "=" "format"
    "a" "code actions"
    "g" "goto"
    "G" "goto (other window)"
    "p" "peek"
    "b" "backend"
    "r" "refactor"
    "h" "help"
    "F" "folders"
    "x" "text/code"
    )

  (leader-set-keys-for-major-mode mode
    ;; format
    "=b" #'lsp-format-buffer
    "=r" #'lsp-format-region
    "=o" #'lsp-organize-imports
    ;; code actions
    "aa" #'lsp-execute-code-action
    ;; format
    ;; "rb" #'lsp-format-buffer
    ;; "rr" #'lsp-format-region
    ;; "rR" #'lsp-rename
    "rr" #'lsp-rename

    ;; goto
    "gt" #'lsp-find-type-definition
    "gd" #'xref-find-definitions
    "gr" #'xref-find-references
    "gh" #'lsp-treemacs-call-hierarchy
    ;; "gr" #'lsp-find-references
    "ge" #'lsp-treemacs-errors-list
    "gb" #'xref-pop-marker-stack
    "gD" #'lsp-find-declaration
    "gf" #'xref-find-definitions-other-frame
    "gi" #'lsp-find-implementation
    "gk" #'petmacs/lsp-avy-goto-word
    "gK" #'petmacs/lsp-avy-goto-symbol
    "gs" #'lsp-ivy-workspace-symbol
    "gS" #'lsp-ivy-global-workspace-symbol
    "gM" #'lsp-ui-imenu
    ;; goto other window
    "Gr" #'petmacs/lsp-find-references-other-window
    "Gt" #'petmacs/lsp-find-type-definition-other-window
    "Gd" #'xref-find-definitions-other-window
    "GD" #'petmacs/lsp-find-declaration-other-window
    "Gi" #'petmacs/lsp-find-implementation-other-window
    "GS" #'lsp-treemacs-symbols
    ;; peek
    "pd" #'lsp-ui-peek-find-definitions
    "pi" #'lsp-ui-peek-find-implementation
    "ps" #'lsp-ui-peek-find-workspace-symbol
    "pb" #'lsp-ui-peek-jump-backward
    "pn" #'lSp-ui-peek-jump-forward
    "pe" #'lsp-ui-flycheck-list
    "pr" #'lsp-ui-peek-find-references
    "pRn" #'lsp-ui-find-next-reference
    "pRp" #'lsp-ui-find-prev-reference
    ;; backend
    "bd" #'lsp-describe-session
    "br" #'lsp-restart-workspace
    "bs" #'lsp-shutdown-workspace
    "bv" #'lsp-version

    "hh" #'lsp-describe-thing-at-point

    ;; text/code
    "xh" #'lsp-document-highlight
    "xl" #'lsp-lens-show
    "xL" #'lsp-lens-hide

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
    "Fa" #'lsp-workspace-folders-add
    )
  )


;;; python mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leader-declare-prefix-for-major-mode 'python-mode
  "c" "compile"
  "v" "virtual environment"
  "vp" "pipenv"
  "t" "test")

(leader-set-keys-for-major-mode 'python-mode
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

  "va" #'pyvenv-activate
  "vd" #'pyvenv-deactivate
  "vw" #'pyvenv-workon

  "vpa" #'pipenv-activate
  "vpd" #'pipenv-deactivate
  "vpi" #'pipenv-install
  "vpo" #'pipenv-open
  "vps" #'pipenv-shell
  "vpu" #'pipenv-uninstall)


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
    "eC" #'petmacs/eval-current-form
    "ee" #'eval-last-sexp
    "er" #'eval-region
    "ef" #'eval-defun
    "gG" #'petmacs/nav-find-elisp-thing-at-point-other-window
    "tq" #'ert))

;;; c-c++ mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (dolist (mode '(c-mode c++-mode))
;;   (leader-declare-prefix-for-major-mode mode
;;     "gh" "hierarchy"
;;     "gm" "members"
;;     )

;;   (leader-set-keys-for-major-mode mode
;;     "bf"  #'ccls-reload
;;     "bp"  #'ccls-preprocess-file
;;     "ghc" #'ccls-call-hierarchy
;;     "ghC" #'petmacs/c-c++-lsp-ccls-call-hierarchy-inv
;;     "ghi" #'ccls-inheritance-hierarchy
;;     "ghI" #'petmacs/c-c++-lsp-ccls-inheritance-hierarchy-inv

;;     "gmh" #'ccls-member-hierarchy
;;     ))

(provide 'init-leader)

;;; init-leader.el ends here
