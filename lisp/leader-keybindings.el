;; leader-keybindings.el --- Setup evil-leader keybindings.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(evil-leader/set-leader petmacs-evil-leader-key)
(evil-major-leader/set-leader petmacs-evil-major-leader-key)

(evil-leader/set-key
  "'"   'petmacs/pop-eshell
  "?"   'counsel-descbinds
  "/"   'counsel-projectile-rg
  "v"   'er/expand-region
  "u"   'universal-argument
  "Ts"  'counsel-load-theme
  "Tn"  'petmacs/cycle-theme
  "TF"  'toggle-frame-fullscreen
  "TM"  'maximize-window
  "TAB" 'petmacs/alternate-buffer
  "d"   'xref-pop-marker-stack)

;; leader-a application family
(which-key-add-key-based-replacements
  (format "%s a" petmacs-evil-leader-key) "application")
(which-key-add-key-based-replacements
  (format "%s ao" petmacs-evil-leader-key) "org")
(which-key-add-key-based-replacements
  (format "%s aC" petmacs-evil-leader-key) "clock")
(evil-leader/set-key
  "ad" 'deer
  "ap"  'list-processes
  "aP"  'proced
  "ar" 'ranger
  "ak"  'paradox-list-packages
  "ao#" 'org-agenda-list-stuck-projects
  "ao/" 'org-occur-in-agenda-files
  "aoa" 'org-agenda-list
  "aoc" 'org-capture
  "aoe" 'org-store-agenda-views
  "aop" 'org-projectile/capture
  "aofi" 'org-feed-goto-inbox
  "aofu" 'org-feed-update-all
  "aoCc" 'org-clock-cancel
  "aoCg" 'org-clock-goto
  "aoCi" 'org-clock-in
  "aoCI" 'org-clock-in-last
  "aoCj" 'petmacs/org-clock-jump-to-current-clock
  "aoCo" 'org-clock-out
  "aoCr" 'org-resolve-clocks
  "aol" 'org-store-link
  "aom" 'org-tags-view
  "aoo" 'org-agenda
  "aos" 'org-search-view
  "aot" 'org-todo-list)
(if sys/win32p
    (evil-leader/set-key
      "am" 'petmacs/open-mintty-terminal-here))

;; leader-q family
(which-key-add-key-based-replacements
  (format "%s q" petmacs-evil-leader-key) "quit")
(evil-leader/set-key
  "qq" 'petmacs/frame-killer
  "qQ" 'kill-emacs
  "qh" 'suspend-frame
  "qR" 'restart-emacs)

;; leader-h family
(which-key-add-key-based-replacements
  (format "%s h" petmacs-evil-leader-key) "helps")
(evil-leader/set-key
  "hdf" 'counsel-describe-function
  "hdv" 'counsel-describe-variable)

;; leader-s search family
(which-key-add-key-based-replacements
  (format "%s s" petmacs-evil-leader-key) "search")
(evil-leader/set-key
  "se" 'evil-iedit-state/iedit-mode)

;; leader-f family
(which-key-add-key-based-replacements
  (format "%s f" petmacs-evil-leader-key) "files")
(which-key-add-key-based-replacements
  (format "%s fy" petmacs-evil-leader-key) "copy")
(which-key-add-key-based-replacements
  (format "%s fv" petmacs-evil-leader-key) "variable")
(evil-leader/set-key
  "ff"  'counsel-find-file
  "fj"  'dired-jump
  "ft"  'treemacs
  "fB"  'treemacs-bookmark
  "fT"  'treemacs-find-file
  "fL"  'counsel-locate
  "fr"  'counsel-recentf
  "fR"  'petmacs/rename-current-buffer-file
  "fs"  'save-buffer
  "fS"  'evil-write-all
  "fc"  'petmacs/copy-file
  "fi"  'insert-file
  "fb"  'counsel-bookmark
  "fB"  'treemacs-bookmark
  "fvd" 'add-dir-local-variable
  "fvf" 'add-file-local-variable
  "fvp" 'add-file-local-variable-prop-line
  "fCu" 'dos2unix
  "fCd" 'unix2dos
  "fCr" 'petmacs/save-buffer-gbk-as-utf8
  "fyy" 'petmacs/copy-file-path
  "fyY" 'petmacs/projectile-copy-file-path
  "fyd" 'petmacs/copy-directory-path
  "fyn" 'petmacs/copy-file-name
  "feo" 'petmacs/find-org-global-todos
  "fec" 'petmacs/find-custom-file
  "fed" 'petmacs/find-dotfile)

;; leader-g family
(which-key-add-key-based-replacements
  (format "%s g" petmacs-evil-leader-key) "git")
(evil-leader/set-key
  "gc"  'magit-clone
  "gff" 'magit-find-file
  "gfl" 'magit-log-buffer-file
  "gfd" 'magit-diff
  "gi"  'magit-init
  "gl"  'magit-log-head
  "gL"  'magit-list-repositories
  "gm"  'magit-dispatch
  "gs"  'magit-status
  "gS"  'magit-stage-file
  "gU"  'magit-unstage-file
  "gho" 'browse-at-remote)

;; leader-i insert family
(which-key-add-key-based-replacements
  (format "%s i" petmacs-evil-leader-key) "insert")
(evil-leader/set-key
  "is" 'ivy-yasnippet)

;; leader-p family
(which-key-add-key-based-replacements
  (format "%s p" petmacs-evil-leader-key) "project")
(evil-leader/set-key
  "p SPC" 'counsel-projectile
  "p'"    'petmacs/projectile-pop-eshell
  "pt"    'petmacs/treemacs-project-toggle
  "pb"    'counsel-projectile-switch-to-buffer
  "pd"    'counsel-projectile-find-dir
  "pp"    'counsel-projectile-switch-project
  "pf"    'counsel-projectile-find-file
  "pr"    'projectile-recentf
  "po"    'org-projectile/goto-todos
  "pl"    'petmacs/ivy-persp-switch-project
  "pv"    'projectile-vc)

;; leader-j family
(which-key-add-key-based-replacements
  (format "%s j" petmacs-evil-leader-key) "jumps")
(evil-leader/set-key
  "ji" 'petmacs/counsel-jump-in-buffer
  "jw" 'evil-avy-goto-word-or-subword-1
  "jD" 'deer-jump-other-window
  "jd" 'deer
  "jj" 'avy-goto-char-timer
  "jJ" 'avy-goto-char-2)

;; leader-e family
(which-key-add-key-based-replacements
  (format "%s e" petmacs-evil-leader-key) "error")
(evil-leader/set-key
  "eb" 'flycheck-buffer
  "ec" 'flycheck-clear
  "eh" 'flycheck-describe-checker
  "el" 'petmacs/toggle-flycheck-error-list
  "en" 'petmacs/next-error
  "eN" 'petmacs/previous-error
  "ep" 'petmacs/previous-error
  "es" 'flycheck-select-checker
  "eS" 'flycheck-set-checker-executable
  "ev" 'flycheck-verify-setup
  "ey" 'flycheck-copy-errors-as-kill
  "ex" 'flycheck-explain-error-at-point)

;; leader-b family
(which-key-add-key-based-replacements
  (format "%s b" petmacs-evil-leader-key) "buffers")
(evil-leader/set-key
  "bb" 'ivy-switch-buffer
  "bd" 'kill-this-buffer
  "bR" 'petmacs/revert-this-buffer
  "bs" 'petmacs/goto-scratch-buffer
  "bx" 'kill-buffer-and-window
  "bh" 'petmacs/goto-dashboard
  "bY" 'petmacs/copy-whole-buffer-to-clipboard
  "ba" 'persp-add-buffer
  "br" 'persp-remove-buffer
  "bj" 'ace-window
  ;; "bi" 'imenu-list-smart-toggle
  "bi" 'lsp-ui-imenu
  "bI" 'ibuffer)

;; leader-t family
(which-key-add-key-based-replacements
  (format "%s t" petmacs-evil-leader-key) "toggle")
(evil-leader/set-key
  "t-" 'centered-cursor-mode
  "ts" 'flycheck-mode
  "tf" 'focus-mode)

;; leader-w family
(which-key-add-key-based-replacements
  (format "%s w" petmacs-evil-leader-key) "windows")
(evil-leader/set-key
  "w."  'hydra-frame-window/body
  "wc"  'olivetti-mode
  "wd"  'delete-window
  "wD"  'ace-delete-window
  "wF"  'make-frame
  "wpm" 'petmacs/shackle-popup-message-buffer
  "wpc" 'petmacs/shackle-popup-compilation-buffer
  "wpl" 'petamcs/shackle-popup-last-buffer
  )

;; leader-o family
(which-key-add-key-based-replacements
  (format "%s o" petmacs-evil-leader-key) "yours")
(evil-leader/set-key
  "ow"  'whitespace-cleanup
  ;; bookmarks
  "obs" 'bookmark-set
  "obd" 'bookmark-delete
  "obr" 'bookmark-rename
  "obl" 'bookmark-bmenu-list)

;;;; major mode specific keybinding

(which-key-add-key-based-replacements
  (format "%s m" petmacs-evil-leader-key) "major mode cmds")

(defun petmacs//set-key-prefix-name (key name)
  (which-key-add-key-based-replacements (format "%s m%s" petmacs-evil-leader-key key) name)
  (which-key-add-key-based-replacements (format ", %s" key) name))

(petmacs//set-key-prefix-name "c" "compile")
(petmacs//set-key-prefix-name "d" "debug")
(petmacs//set-key-prefix-name "g" "goto")
(petmacs//set-key-prefix-name "s" "REPL")
(petmacs//set-key-prefix-name "h" "help")
(petmacs//set-key-prefix-name "v" "virtualenv")
(petmacs//set-key-prefix-name "vp" "pipenv")

;;; json
(evil-leader/set-key-for-mode 'json-mode "m=" 'prettier-js)

;;; python
(evil-leader/set-key-for-mode 'python-mode
  "m=" 'yapfify-buffer
  "m=" 'yapfify-buffer
  "m." 'petmacs/python-load-venv-file
  "mcc" 'petmacs/python-execute-file
  "mcC" 'petmacs/python-execute-file-focus
  "mck" 'petmacs/quit-subjob
  "mdb" 'petmacs/python-insert-breakpoint
  "mdd" 'petmacs/python-delete-breakpoint
  "mdh" 'petmacs/python-highlight-breakpoint
  "mga" 'anaconda-mode-find-assignments
  "mgg" 'anaconda-mode-find-definitions
  "mgG" 'anaconda-mode-find-definitions-other-window
  "mgu" 'anaconda-mode-find-references
  "mhh" 'anaconda-mode-show-doc
  "mri" 'petmacs/python-remove-unused-imports
  "mrI" 'py-isort-buffer
  "msB" 'petmacs/python-shell-send-buffer-switch
  "msb" 'petmacs/python-shell-send-buffer
  "msb" 'petmacs/python-shell-send-buffer
  "msF" 'petmacs/python-shell-send-defun-switch
  "msf" 'petmacs/python-shell-send-defun
  "msi" 'petmacs/python-start-or-switch-repl
  "msr" 'petmacs/python-shell-send-region
  "msR" 'petmacs/python-shell-send-region-switch
  "msk" 'petmacs/python-interrupt-repl
  "msq" 'petmacs/python-quit-repl
  "mva" 'pyvenv-activate
  "mvd" 'pyvenv-deactivate
  "mvw" 'pyvenv-workon
  "mvpa" 'pipenv-activate
  "mvpd" 'pipenv-deactivate
  "mvpi" 'pipenv-install
  "mvpo" 'pipenv-open
  "mvps" 'pipenv-shell
  "mvpu" 'pipenv-uninstall)

;;; markdown
(evil-leader/set-key-for-mode 'markdown-mode
  ;; Movement
  "m{"   'markdown-backward-paragraph
  "m}"   'markdown-forward-paragraph
  ;; Completion, and Cycling
  "m]"   'markdown-complete
  ;; Indentation
  "m>"   'markdown-indent-region
  "m<"   'markdown-outdent-region
  ;; Buffer-wide commands
  "mc]"  'markdown-complete-buffer
  "mcc"  'markdown-check-refs
  "mce"  'markdown-export
  "mcm"  'markdown-other-window
  "mcn"  'markdown-cleanup-list-numbers
  ;; "mco"  'markdown-open
  "mco"  'petmacs/open-markdown-in-typora
  "mcp"  'markdown-preview
  "mcv"  'markdown-export-and-preview
  "mcw"  'markdown-kill-ring-save
  ;; headings
  "mhi"  'markdown-insert-header-dwim
  "mhI"  'markdown-insert-header-setext-dwim
  "mh1"  'markdown-insert-header-atx-1
  "mh2"  'markdown-insert-header-atx-2
  "mh3"  'markdown-insert-header-atx-3
  "mh4"  'markdown-insert-header-atx-4
  "mh5"  'markdown-insert-header-atx-5
  "mh6"  'markdown-insert-header-atx-6
  "mh!"  'markdown-insert-header-setext-1
  "mh@"  'markdown-insert-header-setext-2
  ;; Insertion of common elements
  "m-"   'markdown-insert-hr
  "mif"  'markdown-insert-footnote
  "mii"  'markdown-insert-image
  "mik"  'petmacs/insert-keybinding-markdown
  "mil"  'markdown-insert-link
  "miw"  'markdown-insert-wiki-link
  "miu"  'markdown-insert-uri
  ;; Element removal
  "mk"   'markdown-kill-thing-at-point
  ;; List editing
  "mli"  'markdown-insert-list-item
  ;; Toggles
  "mti"  'markdown-toggle-inline-images
  "mtm"  'markdown-toggle-markup-hiding
  "mtl"  'markdown-toggle-url-hiding
  "mtt"  'markdown-toggle-gfm-checkbox
  "mtw"  'markdown-toggle-wiki-links
  ;; region manipulation
  "mxb"  'markdown-insert-bold
  "mxi"  'markdown-insert-italic
  "mxc"  'markdown-insert-code
  "mxC"  'markdown-insert-gfm-code-block
  "mxq"  'markdown-insert-blockquote
  "mxQ"  'markdown-blockquote-region
  "mxp"  'markdown-insert-pre
  "mxP"  'markdown-pre-region
  ;; Following and Jumping
  "mN"   'markdown-next-link
  "mf"   'markdown-follow-thing-at-point
  "mP"   'markdown-previous-link
  "m <RET>" 'markdown-do
  "mit" 'markdown-toc-generate-toc
  "mcP" 'vmd-mode)

;;; org mode
(evil-leader/set-key-for-mode 'org-mode
  ;; Movement
  ;; "mC"   'evil-org-recompute-clocks
  "m'" 'org-edit-special
  "mc" 'org-capture

  ;; Clock
  ;; These keybindings should match those under the "aoC" prefix (below)
  "mCc" 'org-clock-cancel
  "mCd" 'org-clock-display
  "mCe" 'org-evaluate-time-range
  "mCg" 'org-clock-goto
  "mCi" 'org-clock-in
  "mCI" 'org-clock-in-last
  "mCj" 'petmacs/org-clock-jump-to-current-clock
  "mCo" 'org-clock-out
  "mCR" 'org-clock-report
  "mCr" 'org-resolve-clocks
  "mCp" 'org-pomodoro

  "mdd" 'org-deadline
  "mds" 'org-schedule
  "mdt" 'org-time-stamp
  "mdT" 'org-time-stamp-inactive
  "mee" 'org-export-dispatch
  "mfi" 'org-feed-goto-inbox
  "mfu" 'org-feed-update-all

  "ma" 'org-agenda

  "mp" 'org-priority

  "mTc" 'org-toggle-checkbox
  "mTe" 'org-toggle-pretty-entities
  "mTi" 'org-toggle-inline-images
  "mTl" 'org-toggle-link-display
  "mTt" 'org-show-todo-tree
  "mTT" 'org-todo
  "mTV" 'space-doc-mode
  "mTx" 'org-toggle-latex-fragment

  ;; More cycling options (timestamps, headlines, items, properties)
  "mL" 'org-shiftright
  "mH" 'org-shiftleft
  "mJ" 'org-shiftdown
  "mK" 'org-shiftup

  ;; Change between TODO sets
  "m C-S-l" 'org-shiftcontrolright
  "m C-S-h" 'org-shiftcontrolleft
  "m C-S-j" 'org-shiftcontroldown
  "m C-S-k" 'org-shiftcontrolup

  ;; Subtree editing
  "msa" 'org-toggle-archive-tag
  "msA" 'org-archive-subtree
  "msb" 'org-tree-to-indirect-buffer
  "msh" 'org-promote-subtree
  "msj" 'org-move-subtree-down
  "msk" 'org-move-subtree-up
  "msl" 'org-demote-subtree
  "msn" 'org-narrow-to-subtree
  "msN" 'widen
  "msr" 'org-refile
  "mss" 'org-sparse-tree
  "msS" 'org-sort

  ;; tables
  "mta" 'org-table-align
  "mtb" 'org-table-blank-field
  "mtc" 'org-table-convert
  "mtdc" 'org-table-delete-column
  "mtdr" 'org-table-kill-row
  "mte" 'org-table-eval-formula
  "mtE" 'org-table-export
  "mth" 'org-table-previous-field
  "mtH" 'org-table-move-column-left
  "mtic" 'org-table-insert-column
  "mtih" 'org-table-insert-hline
  "mtiH" 'org-table-hline-and-move
  "mtir" 'org-table-insert-row
  "mtI" 'org-table-import
  "mtj" 'org-table-next-row
  "mtJ" 'org-table-move-row-down
  "mtK" 'org-table-move-row-up
  "mtl" 'org-table-next-field
  "mtL" 'org-table-move-column-right
  "mtn" 'org-table-create
  "mtN" 'org-table-create-with-table.el
  "mtr" 'org-table-recalculate
  "mts" 'org-table-sort-lines
  "mttf" 'org-table-toggle-formula-debugger
  "mtto" 'org-table-toggle-coordinate-overlays
  "mtw" 'org-table-wrap-region

  ;; Source blocks / org-babel
  "mbp"     'org-babel-previous-src-block
  "mbn"     'org-babel-next-src-block
  "mbe"     'org-babel-execute-maybe
  "mbo"     'org-babel-open-src-block-result
  "mbv"     'org-babel-expand-src-block
  "mbu"     'org-babel-goto-src-block-head
  "mbg"     'org-babel-goto-named-src-block
  "mbr"     'org-babel-goto-named-result
  "mbb"     'org-babel-execute-buffer
  "mbs"     'org-babel-execute-subtree
  "mbd"     'org-babel-demarcate-block
  "mbt"     'org-babel-tangle
  "mbf"     'org-babel-tangle-file
  "mbc"     'org-babel-check-src-block
  "mbj"     'org-babel-insert-header-arg
  "mbl"     'org-babel-load-in-session
  "mbi"     'org-babel-lob-ingest
  "mbI"     'org-babel-view-src-block-info
  "mbz"     'org-babel-switch-to-session
  "mbZ"     'org-babel-switch-to-session-with-code
  "mba"     'org-babel-sha1-hash
  "mbx"     'org-babel-do-key-sequence-in-edit-buffer
  "mb."     'org-babel-transient-state/body
  ;; Multi-purpose keys
  "m," 'org-ctrl-c-ctrl-c
  "m*" 'org-ctrl-c-star
  "m-" 'org-ctrl-c-minus
  "m#" 'org-update-statistics-cookies
  "m RET"   'org-ctrl-c-ret
  "m M-RET" 'org-meta-return
  ;; attachments
  "mA" 'org-attach
  ;; insertion
  "mib" 'org-insert-structure-template
  "mid" 'org-insert-drawer
  "mie" 'org-set-effort
  "mif" 'org-footnote-new
  "mih" 'org-insert-heading
  "miH" 'org-insert-heading-after-current
  "mii" 'org-insert-item
  "miK" 'petmacs/insert-keybinding-org
  "mil" 'org-insert-link
  "min" 'org-add-note
  "mip" 'org-set-property
  "mis" 'org-insert-subheading
  "mit" 'org-set-tags-command
  ;; region manipulation
  "mxo" 'org-open-at-point
  )

(evil-leader/set-key-for-mode 'org-agenda-mode
  "ma" 'org-agenda
  "mCc" 'org-agenda-clock-cancel
  "mCi" 'org-agenda-clock-in
  "mCo" 'org-agenda-clock-out
  "mCp" 'org-pomodoro
  "mdd" 'org-agenda-deadline
  "mds" 'org-agenda-schedule
  "mie" 'org-agenda-set-effort
  "mip" 'org-agenda-set-property
  "mit" 'org-agenda-set-tags
  "msr" 'org-agenda-refile
  )

(with-eval-after-load 'org-agenda
  (evil-set-initial-state 'org-agenda-mode 'normal)
  (evil-define-key 'normal org-agenda-mode-map
    "j" 'org-agenda-next-line
    "k" 'org-agenda-previous-line
    ;; C-h should not be rebound by evilification so we unshadow it manually
    ;; TODO add the rule in auto-evilification to ignore C-h (like we do
    ;; with C-g)
    (kbd "<RET>") 'org-agenda-switch-to
    (kbd "\t") 'org-agenda-goto

    "q" 'org-agenda-quit
    "r" 'org-agenda-redo
    "S" 'org-save-all-org-buffers
    "gj" 'org-agenda-goto-date
    "gJ" 'org-agenda-clock-goto
    "gm" 'org-agenda-bulk-mark
    "go" 'org-agenda-open-link
    "s" 'org-agenda-schedule
    "+" 'org-agenda-priority-up
    "," 'org-agenda-priority
    "-" 'org-agenda-priority-down
    "y" 'org-agenda-todo-yesterday
    "n" 'org-agenda-add-note
    "t" 'org-agenda-todo
    ":" 'org-agenda-set-tags
    ";" 'org-timer-set-timer
    "I" 'helm-org-task-file-headings
    "i" 'org-agenda-clock-in-avy
    "O" 'org-agenda-clock-out-avy
    "u" 'org-agenda-bulk-unmark
    "x" 'org-agenda-exit
    "j"  'org-agenda-next-line
    "k"  'org-agenda-previous-line
    "vt" 'org-agenda-toggle-time-grid
    "va" 'org-agenda-archives-mode
    "vw" 'org-agenda-week-view
    "vl" 'org-agenda-log-mode
    "vd" 'org-agenda-day-view
    "vc" 'org-agenda-show-clocking-issues
    "g/" 'org-agenda-filter-by-tag
    "o" 'delete-other-windows
    "gh" 'org-agenda-holiday
    "gv" 'org-agenda-view-mode-dispatch
    "f" 'org-agenda-later
    "b" 'org-agenda-earlier
    "c" 'helm-org-capture-templates
    "e" 'org-agenda-set-effort
    "n" nil  ; evil-search-next
    "{" 'org-agenda-manipulate-query-add-re
    "}" 'org-agenda-manipulate-query-subtract-re
    "A" 'org-agenda-toggle-archive-tag
    "." 'org-agenda-goto-today
    "0" 'evil-digit-argument-or-evil-beginning-of-line
    "<" 'org-agenda-filter-by-category
    ">" 'org-agenda-date-prompt
    "F" 'org-agenda-follow-mode
    "D" 'org-agenda-deadline
    "H" 'org-agenda-holidays
    "J" 'org-agenda-next-date-line
    "K" 'org-agenda-previous-date-line
    "L" 'org-agenda-recenter
    "P" 'org-agenda-show-priority
    "R" 'org-agenda-clockreport-mode
    "Z" 'org-agenda-sunrise-sunset
    "T" 'org-agenda-show-tags
    "X" 'org-agenda-clock-cancel
    "[" 'org-agenda-manipulate-query-add
    "g\\" 'org-agenda-filter-by-tag-refine
    "]" 'org-agenda-manipulate-query-subtract

    (kbd "C-h") nil
    (kbd "M-j") 'org-agenda-next-item
    (kbd "M-k") 'org-agenda-previous-item
    (kbd "M-h") 'org-agenda-earlier
    (kbd "M-l") 'org-agenda-later
    (kbd "gd") 'org-agenda-toggle-time-grid
    (kbd "gr") 'org-agenda-redo
    (kbd "M-RET") 'org-agenda-show-and-scroll-up
    (kbd "M-SPC") 'org-agenda-transient-state/body
    (kbd "s-M-SPC") 'org-agenda-transient-state/body
    ))

(with-eval-after-load 'org-capture
  (evil-define-minor-mode-key 'normal 'org-capture-mode
    (kbd "c") 'org-capture-finalize
    (kbd "k") 'org-capture-kill
    (kbd "a") 'org-capture-kill
    (kbd "r") 'org-capture-refile))
(with-eval-after-load 'org-src
  (evil-define-minor-mode-key 'normal 'org-src-mode
    "c" 'org-edit-src-exit
    "a" 'org-edit-src-abort
    "k" 'org-edit-src-abort))

;;;; evil jumps
;;; evil jump in python mode
(evil-define-minor-mode-key 'normal 'anaconda-mode
  (kbd "gd") 'anaconda-mode-find-definitions
  (kbd "gD") 'anaconda-mode-find-definitions-other-window)

;; evil jump in elisp mode
(evil-define-key 'normal emacs-lisp-mode-map (kbd "gD") 'petmacs/evil-goto-definition-other-window)

(provide 'leader-keybindings)

;;; leader-keybindings ends here
