;; -*- lexical-binding: t no-byte-compile: t -*-

(require 'init-custom)
(require 'init-funcs)

;; Optimization
(setq idle-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;; Menu/Tool/Scroll bars
(unless emacs/>=27p
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist))

(use-package winum
  :init
  (setq winum-scope 'visible
        winum-ignored-buffers '(" *which-key*"
                                " *MINIMAP*"))
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
  ;; (define-key winum-keymap (kbd "M-0") 'winum-select-window-0-or-10)
  (define-key winum-keymap (kbd "M-0") 'treemacs-select-window))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package nerd-icons
  :config
  (when (and (display-graphic-p)
             (not (font-installed-p nerd-icons-font-family)))
    (nerd-icons-install-fonts t)))

(use-package nerd-icons-completion
  :when (icons-displayable-p)
  :hook (vertico-mode . nerd-icons-completion-mode))

;; ;; make "unreal" buffers (like popups, sidebars, log buffers,
;; ;; terminals by giving the latter a slightly different (often darker) background
(use-package solaire-mode
  :hook (after-load-theme . solaire-global-mode))

(use-package srcery-theme)
(use-package standard-themes)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  (with-no-warnings
    (defun my-doom-themes-visual-bell-fn ()
      "Blink the mode-line red briefly. Set `ring-bell-function' to this to use it."
      (let ((buf (current-buffer))
            (cookies (mapcar (lambda (face)
                               (face-remap-add-relative face 'doom-themes-visual-bell))
                             '(mode-line mode-line-active))))
        (force-mode-line-update)
        (run-with-timer 0.15 nil
                        (lambda ()
                          (with-current-buffer buf
                            (mapc #'face-remap-remove-relative cookies)
                            (force-mode-line-update))))))
    (advice-add #'doom-themes-visual-bell-fn :override #'my-doom-themes-visual-bell-fn))
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package modus-themes
  :init
  (setq modus-themes-bold-constructs t
        ;; modus-themes-italic-constructs t
        modus-themes-org-blocks 'tinted-background
        ;; modus-themes-syntax '(yellow-comments green-strings)
        modus-themes-hl-line '(intense)

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

(use-package display-time
  :ensure nil
  ;; :hook (after-init . display-time-mode)
  :init
  (setq display-time-interval 1
        display-time-24hr-format t
        display-time-day-and-date t
        display-time-format "%m-%d %H:%M %a"
        display-time-default-load-average nil))

(when (and sys/mac-ns-p sys/mac-x-p)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-hook 'after-load-theme-hook
            (lambda ()
              (let ((bg (frame-parameter nil 'background-mode)))
                (set-frame-parameter nil 'ns-appearance bg)
                (setcdr (assq 'ns-appearance default-frame-alist) bg)))))


(use-package awesome-tray
  :ensure nil
  :commands (awesome-tray-update)
  :hook (after-init . awesome-tray-mode)
  :custom-face (awesome-tray-module-belong-face ((((background light)) :inherit petmacs-favor-color-face)
                                                 (t (:inherit petmacs-favor-color-face))))
  :init
  (setq awesome-tray-separator " ┃ "
        awesome-tray-hide-mode-line nil
        awesome-tray-info-padding-right 2
        awesome-tray-update-interval 0.5
        awesome-tray-belong-update-duration 2.5
        awesome-tray-date-format "%m-%d %H:%M %a"
        awesome-tray-git-format "%s"
        awesome-tray-git-show-status nil
        awesome-tray-active-modules   '("pomodoro" "flymake" "pyvenv" "git" "date")
        awesome-tray-essential-modules '("pomodoro" "date"))
  :config
  (defun awesome-tray-module-pomodoro-info () (format "%s" pomodoro-mode-line-string))
  (defface awesome-tray-module-pomodoro-face
    '((((background light))
       :foreground "#008080" :bold t)
      (t
       :foreground "#00ced1" :bold t))
    "pomodoro face."
    :group 'awesome-tray)
  (add-to-list 'awesome-tray-module-alist '("pomodoro" . (awesome-tray-module-pomodoro-info awesome-tray-module-pomodoro-face)))

  (defun awesome-tray-module-pyvenv-info ()
    (if (and (member major-mode '(python-mode python-ts-mode)) (bound-and-true-p pyvenv-workon))
        (format "<%s>" pyvenv-workon)
      ""))
  (defface awesome-tray-module-pyvenv-face
    '((((background light))
       :foreground "#0673d8" :bold t)
      (t
       :foreground "#369bf7" :bold t))
    "pyvenv face."
    :group 'awesome-tray)
  (add-to-list 'awesome-tray-module-alist '("pyvenv" . (awesome-tray-module-pyvenv-info awesome-tray-module-pyvenv-face)))

  (defun petmacs/awesome-tray-module-flymake-info ()
    "A module for showing Flymake state.(use custom unicode)"
    ;; Parts of the code are from doom-modeline package
    (with-demoted-errors
        ""
      (if (and (featurep 'flymake) flymake--state)
          (let* ((known (hash-table-keys flymake--state))
                 (running (flymake-running-backends))
                 (disabled (flymake-disabled-backends))
                 (reported (flymake-reporting-backends))
                 (disabledp (and disabled (null running)))
                 (waiting (cl-set-difference running reported)))
            (when-let
                ((flymake-state
                  (cond
                   (waiting "⏳")
                   ((null known) "❔")
                   (disabledp "❕")
                   (t (let ((.error 0)
                            (.warning 0)
                            (.note 0))
                        (cl-loop
                         with warning-level = (warning-numeric-level :warning)
                         with note-level = (warning-numeric-level :debug)
                         for state being the hash-values of flymake--state
                         do (cl-loop
                             with diags = (flymake--state-diags state)
                             for diag in diags do
                             (let ((severity (flymake--lookup-type-property (flymake--diag-type diag) 'severity
                                                                            (warning-numeric-level :error))))
                               (cond ((> severity warning-level) (cl-incf .error))
                                     ((> severity note-level)    (cl-incf .warning))
                                     (t                          (cl-incf .note))))))
                        (let ((num (+ .error .warning .note)))
                          (if (> num 0)
                              (string-clean-whitespace
                               (string-join
                                (list
                                 (when (> .note 0)
                                   (propertize (concat "📖:" (number-to-string .note)) 'face 'awesome-tray-module-flymake-note))
                                 (when (> .warning 0)
                                   (propertize (concat "❗:" (number-to-string .warning)) 'face 'awesome-tray-module-flymake-warning))
                                 (when (> .error 0)
                                   (propertize (concat "❌:" (number-to-string .error)) 'face 'awesome-tray-module-flymake-error)))
                                " "))
                            (propertize "✅" 'face 'awesome-tray-green-face))))))))
              flymake-state)))))
  (advice-add #'awesome-tray-module-flymake-info :override #'petmacs/awesome-tray-module-flymake-info)

  (add-hook 'after-save-hook 'awesome-tray-update))

(use-package doom-modeline
  :hook (awesome-tray-mode . doom-modeline-mode)
  :init
  (setq doom-modeline-icon petmacs-icon
        doom-modeline-buffer-file-name-style 'auto
        doom-modeline-support-imenu t
        doom-modeline-time-icon nil
        doom-modeline-minor-modes nil
        doom-modeline-indent-info nil
        doom-modeline-height 1
        doom-modeline-mode-alist nil

        doom-modeline-window-width-limit 110
        doom-modeline-env-version nil)

  ;; Prevent flash of unstyled modeline at startup
  (unless after-init-time
    (setq-default mode-line-format nil))
  :config
  (doom-modeline-def-modeline 'my-simple-line
    '(eldoc bar workspace-name window-number modals matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
    '(compilation objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp input-method indent-info buffer-encoding process checker))

  ;; Set default mode-line
  (add-hook 'doom-modeline-mode-hook
            (lambda ()
              (doom-modeline-set-modeline 'my-simple-line 'default))))

(use-package hide-mode-line
  :hook (((treemacs-mode
           eshell-mode shell-mode
           term-mode vterm-mode
           embark-collect-mode
           lsp-ui-imenu-mode
           pdf-annot-list-mode) . turn-on-hide-mode-line-mode)
         (dired-mode . (lambda()
                         (and (bound-and-true-p hide-mode-line-mode)
                              (turn-off-hide-mode-line-mode))))))

;; Show native line numbers if possible, otherwise use `linum'
(when petmacs-enable-display-line-numbers
  (if (fboundp 'display-line-numbers-mode)
      (use-package display-line-numbers
        :ensure nil
        :hook ((prog-mode yaml-mode conf-mode) . display-line-numbers-mode)
        :init
        (setq-default display-line-numbers-widen t
                      ;; display-line-numbers 'visual
                      ;; display-line-numbers-type 'relative
                      ;; display-line-numbers-current-absolute t
                      )
        (defun petmacs/display-line-numbers-relative ()
          "Show relative line numbers."
          (setq-local display-line-numbers 'visual))

        (defun petmacs/display-line-numbers-absolute ()
          "Show absolute line numbers."
          (setq-local display-line-numbers t))

        ;; (add-hook 'evil-insert-state-entry-hook #'petmacs/display-line-numbers-absolute)
        ;; (add-hook 'evil-insert-state-exit-hook #'petmacs/display-line-numbers-relative)

        ;; Disable line numbers for some modes
        (dolist (mode '(org-mode-hook
                        term-mode-hook
                        shell-mode-hook
                        magit-mode-hook
                        help-mode-hook
                        ibuffer-mode-hook
                        dashboard-mode-hook
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
        :init (setq linum-highlight-in-all-buffersp t)))))

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; Display dividers between windows
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

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
(unless emacs/>=30p
  (use-package iscroll
    :diminish
    :hook (image-mode . iscroll-mode)))

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

;; Fontify symbols representing faces with that face
(use-package fontify-face)

(use-package pretty-code
  :ensure nil
  :commands (pretty-code-add-hook)
  :init
  (use-package prettify-utils
    :ensure nil)

  (pretty-code-add-hook 'python-mode-hook     '(;; (:class "class")
                                                (:lambda "lambda")
    					                        (:def "def")))
  (pretty-code-add-hook 'python-ts-mode-hook     '(;; (:class "class")
                                                   (:lambda "lambda")
    					                           (:def "def")))
  ;; (pretty-code-add-hook 'c-mode-hook     '((:class "class")
  ;;                                          (:struct "struct")))
  ;; (pretty-code-add-hook 'c++-mode-hook     '((:class "class")
  ;;                                            (:struct "struct")))
  (pretty-code-add-hook 'java-mode-hook     '(;; (:class "class")
    					                      (:lambda "lambda")))
  (pretty-code-add-hook 'java-ts-mode-hook     '(;; (:class "class")
    					                         (:lambda "lambda")))
  (pretty-code-add-hook 'scala-mode-hook     '((:def "def")
                                               ;; (:class "class")
                                               ;; (:struct "object")
    					                       (:lambda "lambda")))
  (pretty-code-add-hook 'scala-ts-mode-hook     '((:def "def")
                                                  ;; (:class "class")
                                                  ;; (:struct "object")
    					                          (:lambda "lambda")))
  (pretty-code-add-hook 'go-mode-hook     '((:def "func")))
  (pretty-code-add-hook 'go-ts-mode-hook     '((:def "func")))
  (pretty-code-add-hook 'emacs-lisp-mode-hook '((:def "defun")
						                        (:lambda "lambda"))))

(when petmacs-enable-mini-frame
  (use-package mini-frame
    :hook (after-init . mini-frame-mode)
    :init
    (setq mini-frame-show-parameters '((top    . 0.45)
                                       (width  . 0.7)
                                       (left   . 0.5)
                                       (left-fringe . 4)
                                       (right-fringe . 4)
                                       (height . 15))
          mini-frame-color-shift-step 6
          mini-frame-resize-max-height 25)))

(use-package emojify
  :hook (after-init . global-emojify-mode)
  :init (setq emojify-download-emojis-p t))

(use-package nyan-mode
  :hook (doom-modeline-mode . nyan-mode)
  :init
  (setq nyan-animate-nyancat t
        nyan-wavy-trail t))

(use-package circadian
  :ensure nil
  :commands (circadian-setup)
  :custom (circadian-themes petmacs-day-night-themes)
  :init (circadian-setup)
  :config
  (when (bound-and-true-p awesome-tray-mode)
    (add-hook circadian-after-load-theme-hook #'awesome-tray-enable)))


(provide 'init-ui)
