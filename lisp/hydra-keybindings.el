;; hydra-keybindings.el --- Setup hydra keybindings.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))


(use-package hydra-posframe
  :quelpa (hydra-posframe :fetcher github :repo "Ladicle/hydra-posframe")
  :hook (after-init . hydra-posframe-enable))

(pretty-hydra-define global-window (:foreign-keys warn :quit-key "Q" :exit t)
  ("Shortcuts"
   (("TAB" petmacs/alternate-buffer "previous buffer")
    ("/" counsel-projectile-rg "project rg")
    ("v" er/expand-region "expand region")
    ("d" xref-pop-marker-stack "pop back")
    ("'" petmacs/pop-eshell "pop eshell here"))
   "toggle"
   (("T" global-T/body "theme")
    ("t" global-t/body "toggles"))

   "Quit"
   (("q" global-q/body "quit"))
   ))

(pretty-hydra-define global-q (:exit t :quit-key "q" )
  ("UI"
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



(provide 'hydra-keybindings)

;;; hydra-keybindings.el ends here
