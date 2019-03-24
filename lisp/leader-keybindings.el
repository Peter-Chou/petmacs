;; leader-keybindings.el --- Setup evil-leader keybindings.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

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
  "bR" 'petmacs/revert-current-buffer
  "bs" 'petmacs/goto-scratch-buffer
  "bx" 'kill-buffer-and-window
  "bh" 'petmacs/goto-dashboard
  "bY" 'petmacs/copy-whole-buffer-to-clipboard
  "ba" 'persp-add-buffer
  "br" 'persp-remove-buffer
  "bi" 'imenu-list-smart-toggle
  ;; "bi" 'lsp-ui-imenu
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
  "wc"  'olivetti-mode
  "wd"  'delete-window
  "wF"  'make-frame
  "wpm" 'popwin:messages
  "wpp" 'popwin:close-popup-window)

;; leader-o family
(which-key-add-key-based-replacements
  (format "%s o" petmacs-evil-leader-key) "yours")
(evil-leader/set-key
  "ow"  'whitespace-cleanup
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


;;; json
(evil-leader/set-key-for-mode 'json-mode "m=" 'prettier-js)

;;; python
(evil-leader/set-key-for-mode 'python-mode "m=" 'yapfify-buffer)
(evil-leader/set-key-for-mode 'python-mode "mhh" 'anaconda-mode-show-doc)
(evil-leader/set-key-for-mode 'python-mode "mga" 'anaconda-mode-find-assignments)
(evil-leader/set-key-for-mode 'python-mode "mgg" 'anaconda-mode-find-definitions)
(evil-leader/set-key-for-mode 'python-mode "mgG" 'anaconda-mode-find-definitions-other-window)
(evil-leader/set-key-for-mode 'python-mode "mgu" 'anaconda-mode-find-references)
(evil-leader/set-key-for-mode 'python-mode "msb" 'python-shell-send-buffer)
(evil-leader/set-key-for-mode 'python-mode "mck" 'petmacs/quit-subjob)
(evil-leader/set-key-for-mode 'python-mode "mdb" 'petmacs/python-insert-breakpoint)
(evil-leader/set-key-for-mode 'python-mode "mdd" 'petmacs/python-delete-breakpoint)
(evil-leader/set-key-for-mode 'python-mode "mdh" 'petmacs/python-highlight-breakpoint)
(evil-leader/set-key-for-mode 'python-mode "msk" 'petmacs/python-interrupt-repl)
(evil-leader/set-key-for-mode 'python-mode "msq" 'petmacs/python-quit-repl)
(evil-leader/set-key-for-mode 'python-mode "msr" 'python-shell-send-region)
(evil-leader/set-key-for-mode 'python-mode "mva" 'pyvenv-activate)
(evil-leader/set-key-for-mode 'python-mode "mvd" 'pyvenv-deactivate)
(evil-leader/set-key-for-mode 'python-mode "mvw" 'pyvenv-workon)

;; (if sys/win32p
;;     (progn
;;       (evil-leader/set-key-for-mode 'python-mode "mcc" 'petmacs/windows-python-execute-file)
;;       (evil-leader/set-key-for-mode 'python-mode "mcC" 'petmacs/windows-python-execute-file-focus)
;;       (evil-leader/set-key-for-mode 'python-mode "msi" 'petmacs/windows-python-start-or-switch-repl)
;;       )
;;   (progn
;;     (evil-leader/set-key-for-mode 'python-mode "mcc" 'petmacs/python-execute-file)
;;     (evil-leader/set-key-for-mode 'python-mode "mcC" 'petmacs/python-execute-file-focus)
;;     (evil-leader/set-key-for-mode 'python-mode "msi" 'petmacs/python-start-or-switch-repl)
;;     ))
(evil-leader/set-key-for-mode 'python-mode "mcc" 'petmacs/python-execute-file)
(evil-leader/set-key-for-mode 'python-mode "mcC" 'petmacs/python-execute-file-focus)
(evil-leader/set-key-for-mode 'python-mode "msi" 'petmacs/python-start-or-switch-repl)

;;;; evil jump

;; python mode
(evil-define-minor-mode-key 'normal 'anaconda-mode (kbd "gd") 'anaconda-mode-find-definitions)
(evil-define-minor-mode-key 'normal 'anaconda-mode (kbd "gD") 'anaconda-mode-find-definitions-other-window)

;; python mode
(evil-define-key 'normal emacs-lisp-mode-map (kbd "gD") 'petmacs/evil-goto-definition-other-window)

(provide 'leader-keybindings)

;;; leader-keybindings ends here
