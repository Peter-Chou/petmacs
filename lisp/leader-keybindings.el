
(defvar petmacs-evil-leader-key "<SPC>"
  "evil leader key")

(evil-leader/set-leader petmacs-evil-leader-key)

(evil-leader/set-key
  "'"   'petmacs/pop-eshell
  "?"   'counsel-descbinds
  "/"   'counsel-projectile-rg
  "u"   'universal-argument
  "Ts"  'counsel-load-theme
  "TAB" 'petmacs/alternate-buffer
  "d" 'xref-pop-marker-stack)

;; leader-q family
(which-key-add-key-based-replacements
  (format "%s q" petmacs-evil-leader-key) "quit")

(evil-leader/set-key
  "qq" 'petmacs/frame-killer
  "qQ" 'kill-emacs
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

(evil-leader/set-key
  "ff" 'counsel-find-file
  "fj" 'dired-jump
  "ft"    'treemacs
  "fB"    'treemacs-bookmark
  "fT"    'treemacs-find-file
  "fL"  'counsel-locate
  "fr" 'counsel-recentf
  "fs" 'save-buffer
  "fb" 'counsel-bookmark
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
  "gL"  'magit-list-repositories
  "gm"  'magit-dispatch
  "gs"  'magit-status
  "gS"  'magit-stage-file
  "gU"  'magit-unstage-file)

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
  "pv"  'projectile-vc)

;; leader-j family
(which-key-add-key-based-replacements
  (format "%s j" petmacs-evil-leader-key) "jumps")
(evil-leader/set-key
  "ji" 'counsel-imenu
  "jj" 'avy-goto-char-timer
  "jJ" 'avy-goto-char-2)

;; leader-b family
(which-key-add-key-based-replacements
  (format "%s b" petmacs-evil-leader-key) "buffers")
(evil-leader/set-key
  "bb" 'ivy-switch-buffer
  "bx" 'kill-buffer-and-window
  "bi" 'imenu-list-smart-toggle
  "bI" 'ibuffer)

;; leader-w family
(which-key-add-key-based-replacements
  (format "%s w" petmacs-evil-leader-key) "windows")
(evil-leader/set-key
  "wc" 'olivetti-mode)

;;;; major mode specific keybinding
(which-key-add-key-based-replacements
  (format "%s m" petmacs-evil-leader-key) "major mode cmds")

;;; python

(evil-leader/set-key-for-mode 'python-mode "mcc" 'petmacs/python-execute-file)
(evil-leader/set-key-for-mode 'python-mode "mhh" 'anaconda-mode-show-doc)
(evil-leader/set-key-for-mode 'python-mode "mga" 'anaconda-mode-find-assignments)
(evil-leader/set-key-for-mode 'python-mode "mgg" 'petmacs/jump-to-definition)
(evil-leader/set-key-for-mode 'python-mode "mgG" 'petmacs/jump-to-definition-other-window)
(evil-leader/set-key-for-mode 'python-mode "mgu" 'anaconda-mode-find-references)
(evil-leader/set-key-for-mode 'python-mode "msi" 'petmacs/python-start-or-switch-repl)
(evil-leader/set-key-for-mode 'python-mode "msb" 'python-shell-send-buffer)
(evil-leader/set-key-for-mode 'python-mode "msr" 'python-shell-send-region)

;;;; evil jump

(evil-define-minor-mode-key 'normal 'anaconda-mode (kbd "gd") 'anaconda-mode-find-definitions)

(provide 'leader-keybindings)
