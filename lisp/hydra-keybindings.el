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
   (
    ("a" global-a/body "applications")
    ("T" global-T/body "theme")
    ("g" global-g/body "git")
    ("i" global-i/body "insert")
    ("w" global-w/body "windows")
    )
   ""
   (
    ("t" global-t/body "toggles")
    ("q" global-q/body "quit")
    ("f" global-f/body "file")
    ("F" global-F/body "frame")
    ("n" global-n/body "narrow / widen")
    )
   ""
   (
    ("p" global-p/body "project")
    ("j" global-j/body "jumps")
    ("e" global-e/body "errors")
    ("b" global-b/body "buffers")
    )
   ))

(pretty-hydra-define global-n (:exit t :quit-key "q" )
  ("narrow / widen"
   (("r" narrow-to-region "narrow to region")
    ("f" narrow-to-defun "narrow to function")
    ("w" widen "widen narrowed")
    )))

(pretty-hydra-define global-w (:exit t :quit-key "q" )
  ("windows"
   (("c" olivetti-mode "center buffer")
    ("d" delete-window "delete window")
    ("D" ace-delete-window "ace delete window")

    ("." global-wdot/body "window management")
    )))

(pretty-hydra-define global-wdot (:quit-key "q")
   ("Actions"
    (("TAB" other-window "switch")
     ("x" ace-delete-window "delete" :exit t)
     ("m" ace-delete-other-windows "maximize" :exit t)
     ("s" ace-swap-window "swap" :exit t)
     ("a" ace-select-window "select" :exit t)
     ("f" toggle-frame-fullscreen "fullscreen" :exit t))
    "Resize"
    (("h" shrink-window-horizontally "←")
     ("j" enlarge-window "↓")
     ("k" shrink-window "↑")
     ("l" enlarge-window-horizontally "→")
     ("n" balance-windows "balance"))
    "Split"
    (("b" split-window-right "horizontally")
     ("v" split-window-below "vertically"))
    "Zoom"
    (("+" text-scale-increase "in")
     ("=" text-scale-increase "in")
     ("-" text-scale-decrease "out")
     ("0" (text-scale-increase 0) "reset"))
    "Appearance"
    (("F" set-frame-font "font")
     ("T" petmacs/select-theme "theme"))))

(pretty-hydra-define global-b (:exit t :quit-key "q" )
  ("buffers"
   (("b" ivy-switch-buffer "switch buffer")
    ("d" kill-this-buffer "kill this buffer")
    ("n" next-buffer "next buffer")
    ("p" previous-buffer "previous buffer")
    ("R" petmacs/revert-this-buffer "revert current buffer")
    ("s" petmacs/goto-scratch-buffer "goto scratch buffer")
    ("x" kill-buffer-and-window "kill buffer & window")
    ("h" petmacs/goto-dashboard "go to dashboard")
    ("m" petmacs/switch-to-minibuffer-window "go to mini buffer")
    ("Y" petmacs/copy-whole-buffer-to-clipboard "copy current buffer to clipboard")
    ("j" ace-window "jump to window")
    ("i" imenu-list-smart-toggle "open buffer's imenu list")
    ("B" ibuffer "open Ibuffer")

    ("p" global-bp/body "pop out buffers")
    )))


(pretty-hydra-define global-bp (:exit t :quit-key "q" )
  ("pop out buffers"
   (("m" petmacs/shackle-popup-message-buffer "pop out message buffer")
    ("c" petmacs/shackle-popup-compilation-buffer "pop out compilation buffer")
    )))

(pretty-hydra-define global-e (:exit t :quit-key "q" )
  ("applications"
   (("l" petmacs/toggle-flycheck-error-list "list errors")
    ("n" petmacs/next-error "next error")
    ("p" petmacs/previous-error "previous error")
    ("x" flycheck-explain-error-at-point "explain error at point")
    )))

(pretty-hydra-define global-a (:exit t :quit-key "q" )
  ("applications"
   (("p" list-processes "list processes")
    ("P" proced "process buffer")
    ("u" paradox-upgrade-packages "upgrade packages")
    ("w" whitespace-cleanup "cleanup whitespace")

    ("o" global-ao/body "org")
    ("b" global-ab/body "bookmarks")
    )))


(pretty-hydra-define global-ab (:exit t :quit-key "q" )
  ("bookmarks"
   (("s" bookmark-set "set bookmark")
    ("d" bookmark-delete "delete bookmark")
    ("r" bookmark-rename "rename bookmark")
    ("l" bookmark-bmenu-list "list bookmark menu")
    )))

(pretty-hydra-define global-ao (:exit t :quit-key "q" )
  ("org"
   (("#" org-agenda-list-stuck-projects "org agenda list stuck projects")
    ("/" org-occur-in-agenda-files "org occur in agenda files")
    ("a" org-agenda-list "org agenda list")
    ("c" org-capture "org capture")
    ("e" org-store-agenda-views "org store agenda views")
    ("p" org-projectile/capture "org project capture")
    ("o" org-agenda "org agenda")
    ("t" org-todo-list "org todo list")
    )))

(pretty-hydra-define global-j (:exit t :quit-key "q" )
  ("jumps"
   (("i" petmacs/counsel-jump-in-buffer "counsel imenu")
    ("w" evil-avy-goto-word-or-subword-1 "jump to word first char")
    ("d" deer "deer jump")
    ("D" deer-jump-other-window "deer jump in other window")
    ("l" goto-last-change "goto last change")
    ("j" avy-goto-char-timer "goto char 1")
    ("J" avy-goto-char-2 "goto char 2")
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
  ("toggles"
   (("F" toggle-frame-fullscreen "full screen")
    ("M" maximize-window "maximize window")
    ("-" centered-cursor-mode "center cursor")
    ("e" flycheck-mode "toggle flycheck mode")
    ("f" focus-mode "toggle focus mode")
    ("p" proxy-http-toggle "toggle proxy 127.0.0.1:1080")
    )))

(pretty-hydra-define global-Tf (:quit-key "q" )
  ("font"
   (("=" default-text-scale-decrease "font +")
    ("+" default-text-scale-decrease "font +")
    ("-" default-text-scale-increase "font -")
    )))

(evil-global-set-key 'normal (kbd petmacs-evil-leader-key) 'global-window/body)
;; (global-set-key (kbd petmacs-evil-leader-key) 'global-window/body)


(provide 'hydra-keybindings)

;;; hydra-keybindings.el ends here
