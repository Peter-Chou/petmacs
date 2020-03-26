;; init-dashboard.el --- Setup startup dashboard.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package dashboard
  :custom-face (dashboard-banner-logo-title ((t (:height 1.1 :inherit default))))
  :diminish (dashboard-mode page-break-lines-mode)
  :defines (persp-save-dir persp-special-last-buffer)
  :functions (all-the-icons-faicon
	      all-the-icons-material
	      winner-undo
	      widget-forward)
  :hook ((after-init . dashboard-setup-startup-hook)
  	 (dashboard-mode  . (lambda ()
  			      (display-line-numbers-mode -1)
  			      (hl-line-mode -1)
  			      (setq-local frame-title-format "")
  			      (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))))
  :init
  (setq
   dashboard-banner-logo-title "Petmacs --- Adorable just like A PET"
   dashboard-startup-banner (expand-file-name "img/totoro_banner.png" user-emacs-directory)
   dashboard-center-content t
   dashboard-show-shortcuts nil
   dashboard-items '((recents  . 7)
		     (bookmarks . 5)
		     ;; (projects . 5)
		     (agenda . 5))
   dashboard-set-init-info t
   dashboard-set-file-icons t
   dashboard-set-heading-icons t
   dashboard-heading-icons '((recents   . "file-text")
			     (bookmarks . "bookmark")
			     (agenda    . "calendar")
			     (projects  . "file-directory")
			     (registers . "database"))
   dashboard-set-footer t
   dashboard-footer "  S T A Y   H U N G R Y,   S T A Y   F O O L I S H"
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
    (kbd "{") #'dashboard-previous-section
    )
  (evil-define-key 'normal dashboard-mode-map (kbd "gd") 'widget-button-press)
  (evil-define-key 'normal dashboard-mode-map [mouse-1] 'widget-button-click))

(provide 'init-dashboard)

;;; init-dashboard.el ends here
