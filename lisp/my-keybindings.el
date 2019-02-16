
(defvar petmacs-evil-leader-key "<SPC>"
  "evil leader key")

(evil-leader/set-leader petmacs-evil-leader-key)

(evil-leader/set-key
  "?"   'counsel-descbinds
  "hdf" 'counsel-describe-function
  "hdv" 'counsel-describe-variable
  "hR"  'spacemacs/counsel-search-docs
  "Ts"  'counsel-load-theme
  "TAB" 'petmacs/alternate-buffer)

;; leader-f family
(which-key-add-key-based-replacements
  (format "%s f" petmacs-evil-leader-key) "files")

(evil-leader/set-key
  "ff" 'counsel-find-file
  "fL"  'counsel-locate
  "fr" 'counsel-recentf
  "fs" 'save-buffer
  "fb" 'counsel-bookmark
  "fed" 'petmacs/find-dotfile)

;; leader-g family
(which-key-add-key-based-replacements
  (format "%s g" petmacs-evil-leader-key) "git")
(evil-leader/set-key
  "gs" 'magit-status)

;; leader-p family
(which-key-add-key-based-replacements
  (format "%s p" petmacs-evil-leader-key) "project")
(evil-leader/set-key
  "p SPC" 'counsel-projectile
  "pb"    'counsel-projectile-switch-to-buffer
  "pd"    'counsel-projectile-find-dir
  "pp"    'counsel-projectile-switch-project
  "pf"    'counsel-projectile-find-file
  "pr"    'projectile-recentf)

;; leader-j family
(which-key-add-key-based-replacements
  (format "%s j" petmacs-evil-leader-key) "jumps")
(evil-leader/set-key
  "ji" 'counsel-imenu)

;; leader-b family
(which-key-add-key-based-replacements
  (format "%s b" petmacs-evil-leader-key) "buffers")
(evil-leader/set-key
  "bb" 'ivy-switch-buffer)

(provide 'my-keybindings)
