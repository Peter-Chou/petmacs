;; -*- lexical-binding: t no-byte-compile: t -*-

(use-package dashboard
  :after nerd-icons
  :diminish dashboard-mode
  :functions (nerd-icons-octicon
              winner-undo
              widget-forward)
  :custom-face
  (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
  (dashboard-items-face ((t (:weight normal))))
  (dashboard-no-items-face ((t (:weight normal))))
  :hook (dashboard-mode . (lambda ()
                            ;; No title
                            (setq-local frame-title-format nil)
                            ;; Enable `page-break-lines-mode'
                            (when (fboundp 'page-break-lines-mode)
                              (page-break-lines-mode 1))))
  :init
  (setq
   dashboard-banner-logo-title "Petmacs --- Adorable just like A PET"
   dashboard-startup-banner (or (expand-file-name
                                 (if (display-graphic-p)
                                     "data/pics/petmacs_banner.png"
                                   "data/pics/petmacs_banner.txt")
                                 user-emacs-directory) 'official)
   dashboard-page-separator "\n\f\n"
   dashboard-path-style 'truncate-middle
   dashboard-path-max-length 60
   dashboard-center-content t
   dashboard-show-shortcuts nil
   dashboard-items '((recents  . 7)
                     (projects . 5)
                     (bookmarks . 5))

   dashboard-set-init-info t
   dashboard-set-file-icons t
   dashboard-set-heading-icons t

   dashboard-set-footer t
   dashboard-footer-messages '("Enjoy Emacs, Enjoy Petmacs!")
   dashboard-footer-icon
   (if (icons-displayable-p)
       (nerd-icons-octicon "nf-oct-heart" :height 1.2 :face 'nerd-icons-lred)
     (propertize ">" 'face 'dashboard-footer))
   )
  (dashboard-setup-startup-hook))

(provide 'init-dashboard)
