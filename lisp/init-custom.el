;; init-custom.el --- Define customizations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Customization.
;;

;;; Code:

(eval-when-compile
  (require 'package))

(defgroup petmacs nil
  "petmacs Emacs customization."
  :group 'convenience
  :prefix "petmacs-"
  :link '(url-link :tag "Homepage" "https://github.com/Peter-Chou/petmacs"))

(defgroup petmacs-faces nil
  "Faces for petmacs."
  :group 'petmacs
  :group 'faces
  :prefix "petmacs-"
  :link '(url-link :tag "Homepage" "https://github.com/Peter-Chou/petmacs"))

(defcustom petmacs-icon t
  "Display icons or not."
  :group 'petmacs
  :type 'boolean)

(defcustom petmacs-enable-ligatures t
  "enable ligatures"
  :group 'petmacs
  :type 'boolean)

(defcustom petmacs-disable-modeline nil ;; (display-graphic-p)
  "disable modeline"
  :group 'petmacs
  :type 'boolean)

(defcustom petmacs-completion-style 'childframe
  "minibuffer / childframe"
  :group 'petmacs
  :type '(choice (const :tag "Minibuffer" minibuffer)
                 (const :tag "Child Frame" childframe)))

(defcustom petmacs-lsp-active-modes '(c-mode c-ts-mode c++-mode c++-ts-mode
                                             cmake-mode cmake-ts-mode
				                             python-mode python-ts-mode
				                             java-mode java-ts-mode
				                             scala-mode scala-ts-mode
				                             go-mode go-ts-mode
				                             sh-mode sh-ts-mode bash-ts-mode
                                             dockerfile-mode dockerfile-ts-mode)
  "Primary major modes of the lsp activated modes."
  :group 'petmacs
  :type '(repeat (symbol :tag "Major-Mode")))

(defcustom petmacs-day-night-themes '(("8:00" . catppuccin)
                                      ("20:00"  . catppuccin))
  "day night theme"
  :group 'petmacs
  :type '(alist :key-type (string :tag "Time")
                :value-type (symbol :tag "Theme")))

(defcustom petmacs-font
  ;; "ComicCodeLigatures Nerd Font"
  ;; "MonegoLigatures Nerd Font"
  "JetBrains Mono NL"
  "default font"
  :group 'petmacs
  :type 'string)

(defcustom petmacs-chinese-font "Maple Mono SC NF"
  "chinese font"
  :group 'petmacs
  :type 'string)

(defcustom petmacs-font-size 14.0
  "font size for default"
  :group 'petmacs
  :type 'float)

(defcustom petmacs-ultra-screen-font-size 17.8
  "font size for ultra screen"
  :group 'petmacs
  :type 'float)

(defcustom petmacs-sidebar-width 30
  "sidebar width"
  :group 'petmacs
  :type 'integer)

(defcustom petmacs-ultra-sidebar-width 35
  "sidebar width in ultra screen"
  :group 'petmacs
  :type 'integer)

(defcustom petmacs-proxy "winhost:1080"
  "Set network proxy."
  :group 'petmacs
  :type 'string)

(defcustom petmacs-socks-proxy "127.0.0.1:1080"
  "Set SOCKS proxy."
  :group 'petmacs
  :type 'string)

(defcustom petmacs-favor-color "#F37022"
  "petmacs favor color"
  :group 'petmacs
  :type 'string)

(defface petmacs-favor-color-face
  `((((background light)) :foreground ,petmacs-favor-color :bold t)
    (t :foreground ,petmacs-favor-color :bold t))
  "petmacs favorite color face"
  :group 'petmacs-faces)

(provide 'init-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
