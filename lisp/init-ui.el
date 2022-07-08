;; -*- lexical-binding: t no-byte-compile: t -*-

(require 'init-custom)
(require 'init-funcs)

(use-package display-time
  :ensure nil
  :hook (after-init . display-time-mode)
  :init
  (setq display-time-interval 1
        display-time-24hr-format t
        display-time-default-load-average nil
        display-time-format "%m-%d %H:%M %a"))

(when (and sys/mac-ns-p sys/mac-x-p)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-hook 'after-load-theme-hook
            (lambda ()
              (let ((bg (frame-parameter nil 'background-mode)))
                (set-frame-parameter nil 'ns-appearance bg)
                (setcdr (assq 'ns-appearance default-frame-alist) bg)))))

;; Optimization
(setq idle-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Menu/Tool/Scroll bars
(unless emacs/>=27p
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist))

(use-package winum
  :init
  (winum-mode)
  :config
  (define-key winum-keymap (kbd "M-1") 'winum-select-window-1)
  (define-key winum-keymap (kbd "M-2") 'winum-select-window-2)
  (define-key winum-keymap (kbd "M-3") 'winum-select-window-3)
  (define-key winum-keymap (kbd "M-4") 'winum-select-window-4)
  (define-key winum-keymap (kbd "M-5") 'winum-select-window-5)
  (define-key winum-keymap (kbd "M-6") 'winum-select-window-6)

  (define-key winum-keymap (kbd "M-7") 'winum-select-window-7)
  (define-key winum-keymap (kbd "M-8") 'winum-select-window-8)
  (define-key winum-keymap (kbd "M-9") 'lsp-treemacs-symbols))

(use-package all-the-icons
  :if (and petmacs-icon (display-graphic-p))
  ;; fonts will be installed in ~/.local/share/fonts
  :init (unless (or sys/win32p
                    (daemonp)
                    (font-installed-p "all-the-icons"))
          (all-the-icons-install-fonts t))
  ;; :custom (all-the-icons-scale-factor 1.1)
  :config
  ;; Support more icons
  (let ((extension-icon-alist
         '(("bat"  all-the-icons-alltheicon "terminal" :face all-the-icons-lsilver)
           ("cmd"  all-the-icons-alltheicon "terminal" :face all-the-icons-lsilver)
           ("conf" all-the-icons-octicon "settings"    :v-adjust 0.0 :face all-the-icons-yellow)
           ("eln"  all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-dsilver)
           ("epub" all-the-icons-faicon "book"         :height 1.0 :v-adjust -0.1 :face all-the-icons-green)
           ("exe"  all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-dsilver)
           ("make" all-the-icons-fileicon "gnu"        :face all-the-icons-dorange)
           ("rss"  all-the-icons-octicon "rss"         :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange)
           ("toml" all-the-icons-octicon "settings"    :v-adjust 0.0 :face all-the-icons-yellow)
           ("tsx"  all-the-icons-fileicon "tsx"        :height 1.0 :v-adjust -0.1 :face all-the-icons-cyan-alt)
           ("xpm"  all-the-icons-octicon "file-media"  :v-adjust 0.0 :face all-the-icons-dgreen))))
    (dolist (icon extension-icon-alist)
      (add-to-list 'all-the-icons-extension-icon-alist icon)))

  (let ((regexp-icon-alist
         '(("\\.[bB][iI][nN]$"               all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-yellow)
           ("^config$"                       all-the-icons-octicon "settings"    :v-adjust 0.0 :face all-the-icons-dorange)
           ("\\.\\(ba\\|z\\)shrc$"           all-the-icons-alltheicon "script"   :height 0.9 :face all-the-icons-dpink)
           ("\\.\\(bash\\|zsh\\)*_?profile$" all-the-icons-alltheicon "script"   :height 0.9 :face all-the-icons-dred)
           ("\\.\\(ba\\|z\\)sh_history$"     all-the-icons-alltheicon "script"   :height 0.9 :face all-the-icons-dsilver)
           ("\\.zshenv$"                     all-the-icons-alltheicon "script"   :height 0.9 :face all-the-icons-dred)
           ("Cask\\'"                        all-the-icons-fileicon "elisp"      :height 1.0 :v-adjust -0.2 :face all-the-icons-blue)
           ("NEWS$"                          all-the-icons-faicon "newspaper-o"  :height 0.9 :v-adjust -0.2)
           ("^Rakefile$"                     all-the-icons-alltheicon "ruby-alt" :face all-the-icons-red)
           ("^go.\\(sum\\|mod\\)$"           all-the-icons-fileicon "go"         :face all-the-icons-dpurple))))
    (dolist (icon regexp-icon-alist)
      (add-to-list 'all-the-icons-regexp-icon-alist icon)))

  (let ((mode-icon-alist
         '((xwidget-webkit-mode           all-the-icons-faicon "chrome"          :v-adjust -0.1 :face all-the-icons-blue)
           (bongo-playlist-mode           all-the-icons-material "queue_music"   :height 1.2 :face all-the-icons-green)
           (bongo-library-mode            all-the-icons-material "library_music" :height 1.1 :face all-the-icons-green)
           (simple-mpc-mode               all-the-icons-faicon "music"           :v-adjust -0.1 :face all-the-icons-green)
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
           (gitconfig-mode                all-the-icons-octicon "settings"       :v-adjust 0.0 :face all-the-icons-dorange)
           (forge-topic-mode              all-the-icons-alltheicon "git"         :face all-the-icons-blue)
           (help-mode                     all-the-icons-faicon "info-circle"     :height 1.1 :v-adjust -0.1 :face all-the-icons-purple)
           (helpful-mode                  all-the-icons-faicon "info-circle"     :height 1.1 :v-adjust -0.1 :face all-the-icons-purple)
           (Info-mode                     all-the-icons-faicon "info-circle"     :height 1.1 :v-adjust -0.1)
           (cask-mode                     all-the-icons-fileicon "elisp"         :height 1.0 :v-adjust -0.2 :face all-the-icons-blue)
           (ein:notebooklist-mode         all-the-icons-faicon "book"            :face all-the-icons-lorange)
           (ein:notebook-mode             all-the-icons-fileicon "jupyter"       :height 1.2 :face all-the-icons-orange)
           (ein:notebook-multilang-mode   all-the-icons-fileicon "jupyter"       :height 1.2 :face all-the-icons-dorange)
           (nov-mode                      all-the-icons-faicon "book"            :height 1.0 :v-adjust -0.1 :face all-the-icons-green)
           (gfm-mode                      all-the-icons-octicon "markdown"       :face all-the-icons-lblue)
           (osx-dictionary-mode           all-the-icons-material "library_books" :face all-the-icons-lblue)
           (youdao-dictionary-mode        all-the-icons-material "library_books" :face all-the-icons-lblue)
           (fanyi-mode                    all-the-icons-material "library_books" :face all-the-icons-lblue))))
    (dolist (icon mode-icon-alist)
      (add-to-list 'all-the-icons-mode-icon-alist icon))))

(use-package all-the-icons-completion
  :after marginalia
  :hook ((after-init . all-the-icons-completion-mode)
         (marginalia-mode . all-the-icons-completion-marginalia-setup)))

;; ;; make "unreal" buffers (like popups, sidebars, log buffers,
;; ;; terminals by giving the latter a slightly different (often darker) background
(use-package solaire-mode
  :hook (after-load-theme . solaire-global-mode))

(use-package spacemacs-theme
  :init
  (setq ;; spacemacs-theme-comment-italic t
   spacemacs-theme-org-priority-bold t))

(use-package modus-themes
  :init
  (setq
   modus-themes-bold-constructs t
   ;; modus-themes-italic-constructs t
   modus-themes-org-blocks 'tinted-background
   ;; modus-themes-syntax '(yellow-comments green-strings)

   modus-themes-paren-match '(bold intense)
   ;; modus-themes-mode-line '(accented borderless (height . 0.9))
   modus-themes-mode-line '(accented borderless 3d)
   modus-themes-region '(accented bg-only no-extend)
   modus-themes-completions '((matches . (extrabold))
                              (selection . (semibold accented))
                              (popup . (accented intense)))
   modus-themes-headings ; this is an alist: read the manual or its doc string
   '((1 . (rainbow overline background 1.4))
     (2 . (rainbow background 1.3))
     (3 . (rainbow bold 1.2))
     (t . (semilight 1.1)))))

(use-package doom-themes
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable customized theme
  ;; FIXME: https://github.com/emacs-lsp/lsp-treemacs/issues/89
  (with-eval-after-load 'lsp-treemacs
    (doom-themes-treemacs-config)))

;; load theme
;; (petmacs--load-theme 'doom-acario-light)
;; (petmacs--load-theme 'modus-vivendi) ;; dark theme
(petmacs--load-theme 'modus-operandi) ;; light theme

;; (if (and (display-graphic-p) (not (equal petmacs-lsp-client-mode 'lsp-bridge-mode)))
;;     (use-package awesome-tray
;;       :quelpa (awesome-tray :fetcher github
;;   		                    :repo "manateelazycat/awesome-tray"
;;   		                    :files ("*.el"))
;;       :preface
;;       (defun awesome-tray-module-winum-info ()
;;         (format "%s" (winum-get-number-string)))

;;       (defface awesome-tray-module-winum-face
;;         '((((background light))
;;            :foreground "#0673d7" :bold t)
;;           (t
;;            :foreground "#369bf8" :bold t))
;;         "winum face."
;;         :group 'awesome-tray)

;;       (defun awesome-tray-module-pyvenv-info ()
;;         ;; (if (bound-and-true-p pyvenv-mode)
;;         (if (and (equal major-mode 'python-mode) (bound-and-true-p pyvenv-virtual-env-name))
;;             (format "[%s]" pyvenv-virtual-env-name)
;;           ""))

;;       (defface awesome-tray-module-pyvenv-face
;;         '((((background light))
;;            :foreground "#0673d8" :bold t)
;;           (t
;;            :foreground "#369bf7" :bold t))
;;         "pyvenv face."
;;         :group 'awesome-tray)

;;       (defun awesome-tray-module-pomodoro-info () (format "%s" pomodoro-mode-line-string))

;;       (defface awesome-tray-module-pomodoro-face
;;         '((((background light))
;;            :foreground "#008080" :bold t)
;;           (t
;;            :foreground "#00ced1" :bold t))
;;         "pomodoro face."
;;         :group 'awesome-tray)

;;       :commands (awesome-tray-update)
;;       :hook (after-init . awesome-tray-mode)
;;       :init
;;       (setq
;;        awesome-tray-update-interval 0.6
;;        awesome-tray-buffer-name-max-length 30
;;        awesome-tray-file-path-show-filename t
;;        awesome-tray-buffer-name-buffer-changed t

;;        awesome-tray-active-modules   '("winum" "location" "belong" "pyvenv" "buffer-name" "pomodoro" "date")
;;        awesome-tray-essential-modules '("winum" "location" "belong" "buffer-name"))
;;       :config
;;       (defun petmacs/awesome-tray-update-git-command-cache ()
;;         (let* ((git-info (awesome-tray-process-exit-code-and-output "git" "symbolic-ref" "--short" "HEAD"))
;;                (status (nth 0 git-info))
;;                (result (format "%s" (nth 1 git-info))))
;;           (setq awesome-tray-git-command-cache
;;                 (if (equal status 0)
;;                     (replace-regexp-in-string "\n" "" result)
;;                   ""))
;;           awesome-tray-git-command-cache))
;;       (advice-add #'awesome-tray-update-git-command-cache :override #'petmacs/awesome-tray-update-git-command-cache)

;;       (defun petmacs/awesome-tray-module-buffer-name-info ()
;;         (let (bufname)
;;           (setq bufname (if awesome-tray-buffer-name-buffer-changed
;;                             (if (and (buffer-modified-p)
;;                                      (not (eq buffer-file-name nil)))
;;                                 (concat  awesome-tray-buffer-name-buffer-changed-style (buffer-name))
;;                               (buffer-name))
;;                           (format "%s" (buffer-name))))
;;           (awesome-tray-truncate-string bufname awesome-tray-buffer-name-max-length t)))
;;       (advice-add #'awesome-tray-module-buffer-name-info :override #'petmacs/awesome-tray-module-buffer-name-info)

;;       (with-eval-after-load 'modus-themes
;;         (advice-add #'modus-themes-toggle :after #'awesome-tray-enable))

;;       (add-to-list 'awesome-tray-module-alist '("winum" . (awesome-tray-module-winum-info awesome-tray-module-winum-face)))
;;       (add-to-list 'awesome-tray-module-alist '("pyvenv" . (awesome-tray-module-pyvenv-info awesome-tray-module-pyvenv-face)))
;;       (add-to-list 'awesome-tray-module-alist '("pomodoro" . (awesome-tray-module-pomodoro-info awesome-tray-module-pomodoro-face)))
;;       (add-hook 'buffer-list-update-hook #'awesome-tray-update))
;;   (use-package doom-modeline
;;     :preface
;;     (defun petmacs/auto-toggle-pyvenv-mode ()
;;       (if (equal major-mode 'python-mode)
;;           (unless (member '(pyvenv-mode pyvenv-mode-line-indicator) mode-line-misc-info)
;;             (add-to-list 'mode-line-misc-info '(pyvenv-mode pyvenv-mode-line-indicator)))
;;         (when (member '(pyvenv-mode pyvenv-mode-line-indicator) mode-line-misc-info)
;;           (setq mode-line-misc-info (delete '(pyvenv-mode pyvenv-mode-line-indicator)
;;                                             mode-line-misc-info)))
;;         ))
;;     :hook (after-init . doom-modeline-mode)
;;     :init
;;     (setq doom-modeline-icon petmacs-icon
;;           doom-modeline-support-imenu t
;;           doom-modeline-minor-modes nil

;;           ;; doom-modeline-height 1
;;           doom-modeline-height 0.9
;;           doom-modeline-window-width-limit 90
;;           doom-modeline-buffer-file-name-style 'relative-to-project)
;;     ;; Prevent flash of unstyled modeline at startup
;;     (unless after-init-time
;;       (setq-default mode-line-format nil))
;;     :config
;;     (add-hook 'buffer-list-update-hook #'petmacs/auto-toggle-pyvenv-mode))
;;   )

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-icon petmacs-icon
        doom-modeline-support-imenu t
        doom-modeline-minor-modes nil
        doom-modeline-height 1
        doom-modeline-buffer-file-name-style 'relative-to-project)
  ;; Prevent flash of unstyled modeline at startup
  (unless after-init-time
    (setq-default mode-line-format nil))
  :config
  (doom-modeline-def-segment python-venv
    "python venv"
    (propertize
     (if (and (equal major-mode 'python-mode) (bound-and-true-p pyvenv-workon))
         (format "[%s]" pyvenv-workon)
       "")
     'face (doom-modeline-face 'doom-modeline-buffer-timemachine)))

  (doom-modeline-def-segment pomodoro
    "pomodoro"
    (propertize
     (concat
      doom-modeline-spc (format "%s" pomodoro-mode-line-string) doom-modeline-spc)
     'face (doom-modeline-face 'doom-modeline-urgent)))

  (doom-modeline-def-segment date
    "date"
    (propertize
     (concat
      doom-modeline-spc (format "%s" display-time-string) doom-modeline-spc)
     'face (doom-modeline-face 'doom-modeline-evil-normal-state)))

  (doom-modeline-def-modeline 'dashboard
    '(bar window-number buffer-default-directory-simple)
    '(battery irc mu4e gnus github debug minor-modes input-method pomodoro process date))

  (doom-modeline-def-modeline 'project
    '(bar window-number modals buffer-default-directory)
    '(battery irc mu4e gnus github debug minor-modes input-method pomodoro process date))

  (doom-modeline-def-modeline 'vcs
    '(bar window-number modals matches buffer-info buffer-position parrot selection-info)
    '(battery irc mu4e gnus github debug minor-modes pomodoro buffer-encoding process date))

  (doom-modeline-def-modeline 'info
    '(bar window-number buffer-info info-nodes buffer-position parrot selection-info)
    '(pomodoro buffer-encoding date))

  ;; Define your custom doom-modeline
  (doom-modeline-def-modeline 'petmacs/custom-modeline
    '(bar window-number modals matches buffer-info remote-host buffer-position parrot selection-info)
    ;; misc-info removed from the right part of the modeline
    '(python-venv persp-name github debug repl input-method pomodoro buffer-encoding process vcs date))

  ;; Add to `doom-modeline-mode-hook` or other hooks
  (defun petmacs/setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'petmacs/custom-modeline 'default))

  (add-hook 'doom-modeline-mode-hook 'petmacs/setup-custom-doom-modeline))

(use-package hide-mode-line
  :hook (((
           completion-list-mode
           ;; completion-in-region-mode
           eshell-mode
           shell-mode
           term-mode
           vterm-mode
           pdf-annot-list-mode
           flycheck-error-list-mode) . hide-mode-line-mode)))

;; A minor-mode menu for mode-line
(use-package minions
  :hook (doom-modeline-mode . minions-mode))

;; Show native line numbers if possible, otherwise use `linum'
(if (fboundp 'display-line-numbers-mode)
    (use-package display-line-numbers
      :ensure nil
      :hook ((prog-mode yaml-mode conf-mode) . display-line-numbers-mode)
      :init
      (setq-default display-line-numbers 'visual
                    display-line-numbers-widen t
                    display-line-numbers-type 'relative
                    display-line-numbers-current-absolute t)

      (defun petmacs/display-line-numbers-relative ()
        "Show relative line numbers."
        (setq-local display-line-numbers 'visual))

      (defun petmacs/display-line-numbers-absolute ()
        "Show absolute line numbers."
        (setq-local display-line-numbers t))

      (add-hook 'evil-insert-state-entry-hook #'petmacs/display-line-numbers-absolute)
      (add-hook 'evil-insert-state-exit-hook #'petmacs/display-line-numbers-relative)
      ;; Disable line numbers for some modes
      (dolist (mode '(org-mode-hook
                      term-mode-hook
                      shell-mode-hook
                      magit-mode-hook
                      ibuffer-mode-hook
                      dired-mode-hook
                      treemacs-mode-hook
                      eshell-mode-hook))
        (add-hook mode (lambda () (display-line-numbers-mode 0)))))
  (use-package linum-off
    :demand t
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

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-scratch-message nil)
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; Display dividers between windows
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

;; Easily adjust the font size in all frames
(use-package default-text-scale
  :hook (after-init . default-text-scale-mode))

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
(if (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode t)
  (when (and emacs/>=27p (not sys/macp))
    (use-package good-scroll
      :diminish
      :hook (after-init . good-scroll-mode)
      :bind (([remap next] . good-scroll-up-full-screen)
             ([remap prior] . good-scroll-down-full-screen)))))

;; Smooth scrolling over images
(use-package iscroll
  :diminish
  :hook (image-mode . iscroll-mode))

;; Use fixed pitch where it's sensible
(use-package mixed-pitch
  :diminish)

;; Display ugly ^L page breaks as tidy horizontal lines
(use-package page-break-lines
  :diminish
  :hook (after-init . global-page-break-lines-mode))

(with-no-warnings
  (when sys/macp
    ;; Render thinner fonts
    (setq ns-use-thin-smoothing t)
    ;; Don't open a file in a new frame
    (setq ns-pop-up-frames nil)))

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

;; use font supported ligatures
(when (and emacs/>=28p petmacs-enable-ligatures)
  (use-package composite
    :ensure nil
    :init (defvar composition-ligature-table (make-char-table nil))
    :hook (((prog-mode
             conf-mode nxml-mode markdown-mode help-mode
             shell-mode eshell-mode term-mode vterm-mode)
            . (lambda () (setq-local composition-function-table composition-ligature-table))))
    :config
    ;; support ligatures, some toned down to prevent hang
    (let ((alist
           '((33  . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
             (35  . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
             (36  . ".\\(?:\\(>\\)>?\\)")
             (37  . ".\\(?:\\(%\\)%?\\)")
             (38  . ".\\(?:\\(&\\)&?\\)")
             (42  . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
             ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
             (43  . ".\\(?:\\([>]\\)>?\\)")
             ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
             (45  . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
             ;; (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
             (46  . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
             (47  . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
             ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
             (48  . ".\\(?:x[a-zA-Z]\\)")
             (58  . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
             (59  . ".\\(?:\\(;\\);?\\)")
             (60  . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
             (61  . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
             (62  . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
             (63  . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
             (91  . ".\\(?:\\(|\\)[]|]?\\)")
             ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
             (94  . ".\\(?:\\(=\\)=?\\)")
             (95  . ".\\(?:\\(|_\\|[_]\\)_?\\)")
             (119 . ".\\(?:\\(ww\\)w?\\)")
             (123 . ".\\(?:\\(|\\)[|}]?\\)")
             (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
             (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
      (dolist (char-regexp alist)
        (set-char-table-range composition-ligature-table (car char-regexp)
                              `([,(cdr char-regexp) 0 font-shape-gstring]))))
    (set-char-table-parent composition-ligature-table composition-function-table)))

(use-package pretty-code
  :load-path (lambda () (expand-file-name "site-lisp/local/pretty-code" user-emacs-directory))
  :commands (pretty-code-add-hook)
  :init
  (use-package prettify-utils
    :load-path (lambda () (expand-file-name "site-lisp/local/prettify-utils" user-emacs-directory)))

  (pretty-code-add-hook 'python-mode-hook     '(;; (:class "class")
                                                (:lambda "lambda")
    					                        (:def "def")))
  ;; (pretty-code-add-hook 'c-mode-hook     '((:class "class")
  ;;                                          (:struct "struct")))
  ;; (pretty-code-add-hook 'c++-mode-hook     '((:class "class")
  ;;                                            (:struct "struct")))
  (pretty-code-add-hook 'java-mode-hook     '(;; (:class "class")
    					                      (:lambda "lambda")))
  (pretty-code-add-hook 'scala-mode-hook     '((:def "def")
                                               ;; (:class "class")
                                               ;; (:struct "object")
    					                       (:lambda "lambda")))
  (pretty-code-add-hook 'go-mode-hook     '((:def "func")))
  (pretty-code-add-hook 'emacs-lisp-mode-hook '((:def "defun")
						                        (:lambda "lambda"))))

(if (display-graphic-p)
    (use-package kind-all-the-icons
      :load-path (lambda () (expand-file-name "site-lisp/local/kind-all-the-icons" user-emacs-directory))
      :after corfu
      :init
      (when (icon-displayable-p)
        (require 'kind-all-the-icons)
        (add-to-list 'corfu-margin-formatters #'kind-all-the-icons-margin-formatter)))
  ;; M-x kind-icon-preview-all to reset and preview all icons after installation
  (use-package kind-icon
    :quelpa (kind-icon :fetcher github
  		               :repo "jdtsmith/kind-icon"
  		               :files ("*.el"))
    :after corfu
    :init
    (require 'kind-icon)
    ;; to compute blended backgrounds correctly
    (setq kind-icon-default-face 'corfu-default)
    :config
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

(provide 'init-ui)
