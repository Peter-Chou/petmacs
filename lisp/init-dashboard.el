;; init-dashboard.el --- Setup startup dashboard.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package dashboard
  :hook (dashboard-mode  . (lambda ()
			     (display-line-numbers-mode -1)
			     (hl-line-mode -1)))
  :defer nil
  :init
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-banner-logo-title "Petmacs --- Adorable just like A PET")
  (setq dashboard-startup-banner (expand-file-name "img/totoro_banner.png" user-emacs-directory))
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5)))
  :config
  (dashboard-setup-startup-hook)
  (add-to-list 'dashboard-items '(agenda) t)
  (setq show-week-agenda-p t)
  (evil-define-key 'normal dashboard-mode-map (kbd "RET") 'widget-button-press) 
  (evil-define-key 'normal dashboard-mode-map [down-mouse-1] 'widget-button-click))

(provide 'init-dashboard)

;;; init-dashboard.el ends here
