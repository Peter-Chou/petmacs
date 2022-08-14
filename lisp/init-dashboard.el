;; -*- lexical-binding: t no-byte-compile: t -*-

(use-package dashboard
  :custom-face (dashboard-banner-logo-title ((t (:height 1.1 :inherit default))))
  :diminish dashboard-mode
  :defines (persp-save-dir persp-special-last-buffer)
  :functions (all-the-icons-faicon
	          all-the-icons-material
	          winner-undo
	          widget-forward)
  :hook ((after-init . dashboard-setup-startup-hook)
  	     (dashboard-mode  . (lambda () (setq-local frame-title-format nil))))
  :init
  (setq
   dashboard-banner-logo-title "Petmacs --- Adorable just like A PET"
   dashboard-startup-banner (or (expand-file-name
                                 (if (display-graphic-p)
                                     "data/pics/petmacs_banner.png"
                                   "data/pics/petmacs_banner.txt")
                                 user-emacs-directory) 'official)
   dashboard-page-separator "\n\f\n"
   dashboard-center-content t
   dashboard-show-shortcuts nil
   dashboard-items '((recents  . 7)
		             (projects . 5)
		             (bookmarks . 5)
		             (agenda . 5))
   dashboard-set-init-info t
   dashboard-set-file-icons t
   dashboard-set-heading-icons t
   dashboard-heading-icons '((recents   . "history")
			                 (bookmarks . "bookmark")
			                 (agenda    . "calendar")
			                 (projects  . "file-directory")
			                 (registers . "database"))
   dashboard-set-footer t
   dashboard-footer-messages '("Enjoy Emacs, Enjoy Petmacs!")
   dashboard-footer-icon (cond ((icon-displayable-p)
                                (all-the-icons-faicon "heart"
                                                      :height 1.1
                                                      :v-adjust -0.05
                                                      :face 'error))
                               ((char-displayable-p ?🧡) "🧡 ")
                               (t (propertize ">" 'face 'dashboard-footer)))
   )
  :config
  (evil-define-key 'normal dashboard-mode-map
    (kbd "RET") 'widget-button-press
    (kbd "gd") 'widget-button-press
    [mouse-1] 'widget-button-click
    [tab] 'widget-forward
    [backtab] 'widget-backward
    (kbd "j") 'widget-forward
    (kbd "k") 'widget-backward
    (kbd "gr") #'dashboard-refresh-buffer
    (kbd "}") #'dashboard-next-section
    (kbd "{") #'dashboard-previous-section))

(provide 'init-dashboard)
