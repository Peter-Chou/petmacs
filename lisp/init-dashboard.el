;; init-dashboard.el --- Setup startup dashboard.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package dashboard
  :diminish (dashboard-mode page-break-lines-mode)
  :defines (persp-save-dir persp-special-last-buffer)
  :functions (all-the-icons-faicon
              all-the-icons-material
              open-custom-file
              persp-get-buffer-or-null
              persp-load-state-from-file
              persp-switch-to-buffer
              winner-undo
              widget-forward)
  :hook (
	 (after-init . dashboard-setup-startup-hook)
	 (dashboard-mode  . (lambda ()
			      (display-line-numbers-mode -1)
			      (hl-line-mode -1)
			      (setq-local frame-title-format "")
			      (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))))
  :config
  (setq dashboard-banner-logo-title "Petmacs --- Adorable just like A PET")
  (setq dashboard-center-content t)
  ;; (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-set-init-info t)
  (setq show-week-agenda-p t)
  (setq dashboard-startup-banner (expand-file-name "img/totoro_banner.png" user-emacs-directory))
  (setq dashboard-items '((recents  . 8)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)))

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
  )
  ;; (evil-define-key 'normal dashboard-mode-map (kbd "gd") 'widget-button-press)
  ;; (evil-define-key 'normal dashboard-mode-map [mouse-1] 'widget-button-click))

  (provide 'init-dashboard)

;;; init-dashboard.el ends here
