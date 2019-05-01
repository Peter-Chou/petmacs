;; leader-keybindings.el --- Setup evil-leader keybindings.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-variable))

(evil-leader/set-leader petmacs-evil-leader-key)
(evil-major-leader/set-leader petmacs-evil-major-leader-key)

(evil-leader/set-key
  "'"   'petmacs/pop-eshell
  "?"   'counsel-descbinds
  "/"   'counsel-projectile-rg
  "v"   'er/expand-region
  "u"   'universal-argument
  "Ts"  'counsel-load-theme
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
  "aofi" 'org-feed-goto-inbox
  "aofu" 'org-feed-update-all
  "aoCc" 'org-clock-cancel
  "aoCg" 'org-clock-goto
  "aoCi" 'org-clock-in
  "aoCI" 'org-clock-in-last
  "aoCj" 'org-clock-jump-to-current-clock
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
  ;; "bi" 'imenu-list-smart-toggle
  "bi" 'lsp-ui-imenu
  "bI" 'ibuffer)

;; leader-t family
(which-key-add-key-based-replacements
  (format "%s t" petmacs-evil-leader-key) "toggle")
(evil-leader/set-key
  "t-" 'centered-cursor-mode
  "ts" 'flycheck-mode)

;; leader-w family
(which-key-add-key-based-replacements
  (format "%s w" petmacs-evil-leader-key) "windows")
(evil-leader/set-key
  "w."  'hydra-frame-window/body
  "wc"  'olivetti-mode
  "wd"  'delete-window
  "wD"  'ace-delete-window
  "wF"  'make-frame
  "wpm" 'popwin:messages
  "wpc" 'petmacs/popwin:compilation
  "wpp" 'popwin:close-popup-window)

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
  "mco"  'markdown-open
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
  "mik"  'spacemacs/insert-keybinding-markdown
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
  ;; "cP" 'markdown-live-preview-mode
  )

;;;; evil jumps

;;; evil jump in python mode
(evil-define-minor-mode-key 'normal 'anaconda-mode (kbd "gd") 'anaconda-mode-find-definitions)
(evil-define-minor-mode-key 'normal 'anaconda-mode (kbd "gD") 'anaconda-mode-find-definitions-other-window)

;; evil jump in elisp mode
(evil-define-key 'normal emacs-lisp-mode-map (kbd "gD") 'petmacs/evil-goto-definition-other-window)

(provide 'leader-keybindings)

;;; leader-keybindings ends here
