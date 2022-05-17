;; init-custom.el --- Setup custom.el.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defgroup petmacs nil
  "Centaur Emacs customization."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/Peter-Chou/petmacs")
  )

(defcustom  petmacs-proxy "winhost:1080"
  "Set network proxy."
	:group 'petmacs
  :type 'string)

(defcustom petmacs-icon (or (display-graphic-p) (daemonp))
  "Display icons or not."
  :group 'petmacs
  :type 'boolean)


(defcustom  petmacs-font-size 13.0
  "font size"
	:group 'petmacs
  :type 'integer)

;; (defgroup petmacs-theme 'doom-dracula
;; 	"petmacs defaut theme"
;;   :group 'petmacs
;;   :type '(choice (const :tag "dracula" doom-dracula)
;;                  (const :tag "gruvbox" doom-gruvbox)))
;;
	(provide 'init-custom)

;;; init-custom.el ends here
