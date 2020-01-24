;; hydra-keybindings.el --- Setup hydra keybindings.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))


(pretty-hydra-define global-window (:foreign-keys warn :quit-key "Q" :exit t)
  ("Shortcuts"
   (("TAB" petmacs/alternate-buffer "previous buffer")
    ("/" counsel-projectile-rg "project rg")
    ("v" er/expand-region "expand region")
    ("d" xref-pop-marker-stack "pop back")
    ("'" petmacs/pop-eshell "pop eshell here"))
   "global"
   (("T" global-T/body "theme")
    ("t" global-t/body "toggles")
    ("q" global-q/body "quit")
    ("f" global-f/body "file")
    ("F" global-F/body "frame")
    ("g" global-g/body "git")
    ("i" global-i/body "insert")
    ("p" global-p/body "project")
    ("j" global-j/body "jumps")
    )))

(pretty-hydra-define global-j (:exit t :quit-key "q" )
  ("jumps"
   (("i" petmacs/counsel-jump-in-buffer "counsel imenu")
    ("w" evil-avy-goto-word-or-subword-1 "jump to word first char")
    )))

(pretty-hydra-define global-p (:exit t :quit-key "q" )
  ("project"
   (("SPC" counsel-projectile "open counsel in project")
    ("'" petmacs/projectile-pop-eshell "pop eshell at project root")
    ("b" counsel-projectile-switch-to-buffer "open buffer in project")
    ("d" counsel-projectile-find-dir "open directory in project")
    ("p" counsel-projectile-switch-project "switch project")
    ("f" counsel-projectile-find-file "open file in project")
    ("r" projectile-recentf "recentf in project")
    ("o" org-projectile/goto-todos "project todo file")
    ("v" projectile-vc "open git status in project root")
    )))

(pretty-hydra-define global-i (:exit t :quit-key "q" )
  ("insert"
   (("y" ivy-yasnippet "insert snippet")
    ("i" insert-file "insert file content at point")
    )))

(pretty-hydra-define global-g (:exit t :quit-key "q" )
  ("git"
   (("s" magit-status "git status buffer")
    ("c" magit-clone "git clone")
    ("i" magit-init "git init")
    ("l" magit-log-head "git log at head")
    ("L" magit-list-repositories "list git repositories")
    ("m" magit-dispatch "git commands in mini buffer")
    ("S" magit-stage-file "git stage current file")
    ("U" magit-unstage-file "git unstage current file")
    ("w" browse-at-remote "browse current point in Web")

    ("f" global-gf/body "magit file")
    )))

(pretty-hydra-define global-gf (:exit t :quit-key "q" )
  ("git"
   (("f" magit-find-file "magit find file")
    ("l" magit-log-buffer-file "file git history")
    ("d" magit-diff "magit diff")
    ("b" magit-blame "magit blame")
    )))

(pretty-hydra-define global-F (:exit t :quit-key "q" )
  ("frames"
   (("f" find-file-other-frame "open file in other frame")
    ("d" delete-frame "delete current frame")
    ("D" delete-other-frames "delete other frames")
    ("b" switch-to-buffer-other-frame "switch to buffer in other frame")
    ("B" display-buffer-other-frame "display buffer in other frame")
    ("d" delete-frame "delete current frame")
    ("o" other-frame "switch to other frame")
    ("n" make-frame "new frame")
    )))


(pretty-hydra-define global-f (:exit t :quit-key "q" )
  ("File"
   (("f" counsel-find-file "find file")
    ("r" counsel-buffer-or-recentf "recent buffers / files")
    ("R" petmacs/rename-current-buffer-file "rename current file")
    ("s" save-buffer "save buffer")
    ("S" evil-write-all "save all buffers")
    ("c" petmacs/copy-file "copy current file")
    ("j" dired-jump "view in dired")
   ("t" treemacs "view in treemacs")
   ("b" counsel-bookmark "bookmarks (counsel)")
   ("o" petmacs/find-org-global-todos "global org todo file")
   ("B" treemacs-bookmark "bookmarks (treemacs)")
   ("L" counsel-locate "counsel locate")

   ("v" global-fv/body "add local variables")
   ("C" global-fC/body "dos <-> unix (file)")
   ("p" global-fp/body "copy path")
   ("e" global-fe/body "emacs configs")

    )))


(pretty-hydra-define global-fe (:exit t :quit-key "q" )
  ("emacs configs"
   (("d" petmacs/find-dotfile "open emacs dotfile")
    ("c" petmacs/find-custom-file "open emacs custom file")
    )))

(pretty-hydra-define global-fp (:exit t :quit-key "q" )
  ("copy path"
   (("y" petmacs/copy-file-path "copy whole file path")
    ("n" petmacs/copy-file-name "copy simple file path")
    )))

(pretty-hydra-define global-fC (:exit t :quit-key "q" )
  ("dos <-> unix (file)"
   (("u" dos2unix "dos -> unix")
    ("d" unix2dos "unix -> dos")
    ("r" petmacs/save-buffer-gbk-as-utf8 "Revert buffer with GBK & save as UTF-8")
    )))

(pretty-hydra-define global-fv (:exit t :quit-key "q" )
  ("local variables"
   (("d" add-dir-local-variable "add directory local variable")
    ("f" add-file-local-variable "add file local variable")
    ("p" add-file-local-variable-prop-line "add file local variable to -*- line")
    )))


(pretty-hydra-define global-q (:exit t :quit-key "q" )
  ("Quit"
   (("f" petmacs/frame-killer "kill frame")
    ("Q" kill-emacs "kill Emacs")
   ("h" suspend-frame "minimize frame")
   ("R" restart-emacs "restart Emacs")
    )))

(pretty-hydra-define global-T (:exit t :quit-key "q" )
  ("UI"
   (("s" petmacs/select-theme "select theme")
    ("n" petmacs/cycle-theme "cycle theme")
   ("f" global-Tf/body "font")
    )))


(pretty-hydra-define global-t (:exit t :quit-key "q" )
  ("toggle"
   (("F" toggle-frame-fullscreen "full screen")
    ("M" maximize-window "maximize window")
    )))


(pretty-hydra-define global-Tf (:quit-key "q" )
  ("font"
   (("=" default-text-scale-decrease "font +")
    ("-" default-text-scale-increase "font -")
    )))




(evil-global-set-key 'normal (kbd petmacs-evil-leader-key) 'global-window/body)
;; (global-set-key (kbd petmacs-evil-leader-key) 'global-window/body)



(provide 'hydra-keybindings)

;;; hydra-keybindings.el ends here
