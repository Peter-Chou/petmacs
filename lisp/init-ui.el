;; init-ui.el --- Setup UI.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Font
(defun font-installed-p (font-name)
    "Check if font with FONT-NAME is available."
      (find-font (font-spec :name font-name)))

(when sys/mac-x-p
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-hook 'after-load-theme-hook
            (lambda ()
              (let ((bg (frame-parameter nil 'background-mode)))
                (set-frame-parameter nil 'ns-appearance bg)
                (setcdr (assq 'ns-appearance default-frame-alist) bg)))))

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Menu/Tool/Scroll bars
(unless emacs/>=27p
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist))

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000
      auto-window-vscroll nil
      scroll-preserve-screen-position t)

;; Good pixel line scrolling
(when (and emacs/>=27p
           (not sys/macp))
  (use-package good-scroll
    :diminish
    :hook (after-init . good-scroll-mode)
    :bind (([remap next] . good-scroll-up-full-screen)
           ([remap prior] . good-scroll-down-full-screen))))

;; Smooth scrolling over images
(when emacs/>=26p
  (use-package iscroll
    :diminish
    :hook (image-mode . iscroll-mode)))

;; Use fixed pitch where it's sensible
(use-package mixed-pitch
  :diminish)

;; Child frame
(when (childframe-workable-p)
  (use-package posframe
    :hook (after-load-theme . posframe-delete-all)
    :init
    (with-eval-after-load 'persp-mode
      (add-hook 'persp-load-buffer-functions
                (lambda (&rest _)
                  (posframe-delete-all))))
    :config
    (with-no-warnings
      (defun my-posframe--prettify-frame (&rest _)
        (set-face-background 'fringe nil posframe--frame))
      (advice-add #'posframe--create-posframe :after #'my-posframe--prettify-frame)

      (defun posframe-poshandler-frame-center-near-bottom (info)
        (cons (/ (- (plist-get info :parent-frame-width)
                    (plist-get info :posframe-width))
                 2)
              (/ (plist-get info :parent-frame-height)
                 2))))))

;; Make certain buffers grossly incandescent
;; Must before loading the theme
(use-package solaire-mode
  :hook (after-load-theme . solaire-global-mode))

;; Icons
;; NOTE: Must run `M-x all-the-icons-install-fonts', and install fonts manually on Windows
(use-package all-the-icons

  :if (display-graphic-p)
  :init (unless (or sys/win32p (font-installed-p "all-the-icons"))
          (all-the-icons-install-fonts t))
  :config
  (with-no-warnings
    (defun all-the-icons-reset ()
      "Reset the icons."
      (interactive)
      (dolist (func '(all-the-icons-icon-for-dir
                      all-the-icons-icon-for-file
                      all-the-icons-icon-for-mode
                      all-the-icons-icon-for-url
                      all-the-icons-icon-family-for-file
                      all-the-icons-icon-family-for-mode
                      all-the-icons-icon-family))
        (all-the-icons-cache func))
      (message "Reset all-the-icons")))

  ;; Support more icons
  (let ((extension-icon-alist
         '(("conf" all-the-icons-octicon "settings"    :v-adjust 0.0 :face all-the-icons-yellow)
           ("eln"  all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-dsilver)
           ("epub" all-the-icons-faicon "book"         :height 1.0 :v-adjust -0.1 :face all-the-icons-green)
           ("make" all-the-icons-fileicon "gnu"        :face all-the-icons-dorange)
           ("rss"  all-the-icons-octicon "rss"         :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange)
           ("toml" all-the-icons-octicon "settings"    :v-adjust 0.0 :face all-the-icons-yellow)
           ("tsx"  all-the-icons-fileicon "tsx"        :height 1.0 :v-adjust -0.1 :face all-the-icons-cyan-alt)
           ("xpm"  all-the-icons-octicon "file-media"  :v-adjust 0.0 :face all-the-icons-dgreen))))
    (dolist (icon extension-icon-alist)
      (add-to-list 'all-the-icons-extension-icon-alist icon)))

  (let ((regexp-icon-alist
         '(("Cask\\'"             all-the-icons-fileicon "elisp"      :height 1.0 :v-adjust -0.2 :face all-the-icons-blue)
           ("^Rakefile$"          all-the-icons-alltheicon "ruby-alt" :face all-the-icons-red)
           ("\\.\\(bat\\|cmd\\)$" all-the-icons-alltheicon "terminal" :face all-the-icons-lsilver)
           ("\\go.mod$"           all-the-icons-fileicon "go"         :face all-the-icons-dblue)
           ("\\go.sum$"           all-the-icons-fileicon "go"         :face all-the-icons-dpurple)
           ("\\.[bB][iI][nN]$"    all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-yellow)
           ("NEWS$"               all-the-icons-faicon "newspaper-o"  :height 0.9 :v-adjust -0.2))))
    (dolist (icon regexp-icon-alist)
      (add-to-list 'all-the-icons-regexp-icon-alist icon)))

  (let ((mode-icon-alist
         '((xwidget-webkit-mode           all-the-icons-faicon "chrome"          :v-adjust -0.1 :face all-the-icons-blue)
           (bongo-playlist-mode           all-the-icons-material "queue_music"   :height 1.2 :face all-the-icons-green)
           (bongo-library-mode            all-the-icons-material "library_music" :height 1.1 :face all-the-icons-green)
           (gnus-group-mode               all-the-icons-fileicon "gnu"           :face all-the-icons-silver)
           (gnus-summary-mode             all-the-icons-octicon "inbox"          :height 1.0 :v-adjust 0.0 :face all-the-icons-orange)
           (gnus-article-mode             all-the-icons-octicon "mail"           :height 1.1 :v-adjust 0.0 :face all-the-icons-lblue)
           (message-mode                  all-the-icons-octicon "mail"           :height 1.1 :v-adjust 0.0 :face all-the-icons-lblue)
           (diff-mode                     all-the-icons-octicon "git-compare"    :v-adjust 0.0 :face all-the-icons-lred)
           (flycheck-error-list-mode      all-the-icons-octicon "checklist"      :height 1.1 :v-adjust 0.0 :face all-the-icons-lred)
           (elfeed-search-mode            all-the-icons-faicon "rss-square"      :v-adjust -0.1 :face all-the-icons-orange)
           (elfeed-show-mode              all-the-icons-octicon "rss"            :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange)
           (newsticker-mode               all-the-icons-faicon "rss-square"      :v-adjust -0.1 :face all-the-icons-orange)
           (newsticker-treeview-mode      all-the-icons-faicon "rss-square"      :v-adjust -0.1 :face all-the-icons-orange)
           (newsticker-treeview-list-mode all-the-icons-octicon "rss"            :height 1.1 :v-adjust 0.0 :face all-the-icons-orange)
           (newsticker-treeview-item-mode all-the-icons-octicon "rss"            :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange)
           (conf-mode                     all-the-icons-octicon "settings"       :v-adjust 0.0 :face all-the-icons-yellow)
           (conf-space-mode               all-the-icons-octicon "settings"       :v-adjust 0.0 :face all-the-icons-yellow)
           (forge-topic-mode              all-the-icons-alltheicon "git"         :face all-the-icons-blue)
           (help-mode                     all-the-icons-faicon "info-circle"     :height 1.1 :v-adjust -0.1 :face all-the-icons-purple)
           (helpful-mode                  all-the-icons-faicon "info-circle"     :height 1.1 :v-adjust -0.1 :face all-the-icons-purple)
           (Info-mode                     all-the-icons-faicon "info-circle"     :height 1.1 :v-adjust -0.1)
           (cask-mode                     all-the-icons-fileicon "elisp"         :height 1.0 :v-adjust -0.2 :face all-the-icons-blue)
           (ein:notebooklist-mode         all-the-icons-faicon "book"            :face all-the-icons-lorange)
           (ein:notebook-mode             all-the-icons-fileicon "jupyter"       :height 1.2 :face all-the-icons-orange)
           (ein:notebook-multilang-mode   all-the-icons-fileicon "jupyter"       :height 1.2 :face all-the-icons-dorange)
           (nov-mode                      all-the-icons-faicon "book"            :height 1.0 :v-adjust -0.1 :face all-the-icons-green)
           (gfm-mode                      all-the-icons-octicon "markdown"       :face all-the-icons-lblue))))
    (dolist (icon mode-icon-alist)
      (add-to-list 'all-the-icons-mode-icon-alist icon))))

(use-package font-lock+
  :quelpa
  (font-lock+ :repo "emacsmirror/font-lock-plus" :fetcher github))

(use-package all-the-icons-ivy
  :hook (after-init . all-the-icons-ivy-setup))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  ;; prevent flash of unstyled modeline at startup
  (unless after-init-time
    (setq-default mode-line-format nil))
  (setq
   find-file-visit-truename t  ; display the real names for symlink files
   ;; doom-modeline-height 21
   doom-modeline-lsp t
   doom-modeline-persp-name nil
   doom-modeline-minor-modes t
   doom-modeline-buffer-file-name-style 'relative-to-project
   ;; doom-modeline-buffer-file-name-style 'file-name
   doom-modeline-icon (display-graphic-p)
   doom-modeline-major-mode-icon t
   doom-modeline-major-mode-color-icon t
   doom-modeline-buffer-state-icon t
   doom-modeline-buffer-modification-icon t
   doom-modeline-indent-info nil)
  :config
  (doom-modeline-def-segment petmacs||python-venv
    "The current python virtual environment state."
    (when (eq major-mode 'python-mode)
      (if (eq python-shell-virtualenv-root nil)
	  ""
	(propertize
	 (let ((base-dir-name (file-name-nondirectory (substring python-shell-virtualenv-root 0 -1))))
	   (if (< 10 (length base-dir-name))
	       (format " (%s..)" (substring base-dir-name 0 15))
	     (format " (%s)" base-dir-name)))
	 'face (if (doom-modeline--active) 'doom-modeline-buffer-major-mode)))))

  ;; (doom-modeline-def-modeline 'my-modeline-layout
  ;;   '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
  ;;   ;; '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl input-method indent-info buffer-encoding petmacs||python-venv process vcs checker))
  ;;   '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl input-method indent-info buffer-encoding process vcs checker))

  ;; (defun setup-custom-doom-modeline ()
  ;;   (doom-modeline-set-modeline 'my-modeline-layout 'default))
  )

;; A minor-mode menu for mode-line
(when emacs/>=25.2p
  (use-package minions
    :hook (doom-modeline-mode . minions-mode)))


(use-package srcery-theme)
(use-package ayu-theme)
(use-package color-theme-sanityinc-tomorrow)

(use-package doom-themes
  :defer nil
  :custom-face
  (doom-modeline-buffer-file ((t (:inherit (mode-line bold)))))
  :hook (after-load-theme . (lambda ()
			      (set-face-foreground
			       'mode-line
			       (face-foreground 'default))))
  :custom
  (doom-dark+-blue-modeline t)
  (doom-themes-treemacs-theme "doom-colors")
  :config
  ;; FIXME: @see https://github.com/hlissner/emacs-doom-themes/issues/317.
  (set-face-foreground 'mode-line (face-foreground 'default))

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable customized theme
  ;; FIXME https://github.com/emacs-lsp/lsp-treemacs/issues/89
  (with-eval-after-load 'lsp-treemacs
    (doom-themes-treemacs-config))
  )

(use-package circadian
  :init
  ;; (setq circadian-themes petmac-auto-themes)
  (setq circadian-themes petmacs-auto-themes)
  (circadian-setup))

;; (use-package theme-changer
;;   :init
;;   (require 'theme-changer)
;;   (setq calendar-longitude 121.473701
;; 	calendar-latitude 31.230416)
;;   :config
;;   ;; setq the location here.
;;   ;; (change-theme 'doom-vibrant 'doom-nord-light)
;;   (change-theme 'doom-nord-light 'doom-vibrant )
;;   )

;; (load-theme petmacs--default-theme t)

;;; Disable theme before load a new theme
(defadvice load-theme
    (before theme-dont-propagate activate)
  "Disable theme before load theme."
  (mapc #'disable-theme custom-enabled-themes))


;; (use-package display-line-numbers-mode
;;   :ensure nil
;;   :init
;;   (setq-default display-line-numbers-type 'relative)
;;   (global-display-line-numbers-mode 1))

;; Highlight current line number
;; (use-package hlinum
;;   :defines linum-highlight-in-all-buffersp
;;   :custom-face (linum-highlight-face ((t (:inherit default :background nil :foreground nil))))
;;   :hook (global-linum-mode . hlinum-activate)
;;   :init
;;   (setq linum-highlight-in-all-buffersp t))

;; Show native line numbers if possible, otherwise use `linum'
(if (fboundp 'display-line-numbers-mode)
    (use-package display-line-numbers
      :ensure nil
      :hook (prog-mode . display-line-numbers-mode)
      :init
      (setq-default display-line-numbers-type 'relative)
      )
  (use-package linum-off
    :demand
    :defines linum-format
    :hook (after-init . global-linum-mode)
    :init (setq linum-format "%4d ")
    :config
    ;; Highlight current line number
    (use-package hlinum
      :defines linum-highlight-in-all-buffersp
      :custom-face (linum-highlight-face ((t (:inherit default :background nil :foreground nil))))
      :hook (global-linum-mode . hlinum-activate)
      :init (setq linum-highlight-in-all-buffersp t))))

;; Display Time
(use-package time
  :ensure nil
  :init
  (setq display-time-24hr-format t
        display-time-day-and-date t))

(use-package hide-mode-line
  :hook (((completion-list-mode
           completion-in-region-mode
           flycheck-error-list-mode) . hide-mode-line-mode)))

(use-package yascroll
  :hook (after-init . global-yascroll-bar-mode))

;; Suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;; Display dividers between windows
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

(fset 'yes-or-no-p 'y-or-n-p)
(setq visible-bell t)
(setq inhibit-compacting-font-caches t) ; Don’t compact font caches during GC.

(add-hook 'window-setup-hook #'size-indication-mode)
(use-package composite
  :ensure nil
  :init (defvar composition-ligature-table (make-char-table nil))
  :hook (((prog-mode conf-mode nxml-mode markdown-mode help-mode)
          . (lambda () (setq-local composition-function-table composition-ligature-table))))
  :config
  ;; support ligatures, some toned down to prevent hang
  (when emacs/>=27p
    (let ((alist
           '((33 . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
             (35 . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
             (36 . ".\\(?:\\(>\\)>?\\)")
             (37 . ".\\(?:\\(%\\)%?\\)")
             (38 . ".\\(?:\\(&\\)&?\\)")
             (42 . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
             ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
             (43 . ".\\(?:\\([>]\\)>?\\)")
             ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
             (45 . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
             ;; (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
             (46 . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
             (47 . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
             ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
             (48 . ".\\(?:x[a-zA-Z]\\)")
             (58 . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
             (59 . ".\\(?:\\(;\\);?\\)")
             (60 . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
             (61 . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
             (62 . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
             (63 . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
             (91 . ".\\(?:\\(|\\)[]|]?\\)")
             ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
             (94 . ".\\(?:\\(=\\)=?\\)")
             (95 . ".\\(?:\\(|_\\|[_]\\)_?\\)")
             (119 . ".\\(?:\\(ww\\)w?\\)")
             (123 . ".\\(?:\\(|\\)[|}]?\\)")
             (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
             (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
      (dolist (char-regexp alist)
        (set-char-table-range composition-ligature-table (car char-regexp)
                              `([,(cdr char-regexp) 0 font-shape-gstring]))))
    (set-char-table-parent composition-ligature-table composition-function-table)))

(provide 'init-ui)

;;; init-ui.el ends here
