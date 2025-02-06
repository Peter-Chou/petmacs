;; -*- lexical-binding: t no-byte-compile: t -*-

(eval-when-compile
  (require 'init-custom)
  (require 'init-funcs))

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

(use-package nerd-icons)

(use-package all-the-icons :if (display-graphic-p))

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

(use-package nerd-icons-completion
  :when (icons-displayable-p)
  :hook (vertico-mode . nerd-icons-completion-mode))

;; ;; make "unreal" buffers (like popups, sidebars, log buffers,
;; ;; terminals by giving the latter a slightly different (often darker) background
(use-package solaire-mode
  :hook (after-init . solaire-global-mode))

(use-package catppuccin-theme
  :init (setq catppuccin-flavor 'latte))

(use-package spacemacs-theme
  :init (setq spacemacs-theme-comment-italic t))

(use-package ef-themes
  :init (setq ef-themes-to-toggle '(ef-melissa-light ef-melissa-dark)))

(use-package modus-themes
  :init (setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        ;; fix ugly faces when using themes without doom-themes
        doom-themes-treemacs-enable-variable-pitch nil
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; WORKAROUND: Visual bell on 29+
  ;; @see https://github.com/doomemacs/themes/issues/733
  (with-no-warnings
    (defun my-doom-themes-visual-bell-fn ()
      "Blink the mode-line red briefly. Set `ring-bell-function' to this to use it."
      (let ((buf (current-buffer))
            (cookies (mapcar (lambda (face)
                               (face-remap-add-relative face 'doom-themes-visual-bell))
                             (if (facep 'mode-line-active)
                                 '(mode-line-active solaire-mode-line-active-face)
                               '(mode-line solaire-mode-line-face)))))
        (force-mode-line-update)
        (run-with-timer 0.2 nil
                        (lambda ()
                          (with-current-buffer buf
                            (mapc #'face-remap-remove-relative cookies)
                            (force-mode-line-update))))))
    (advice-add #'doom-themes-visual-bell-fn :override #'my-doom-themes-visual-bell-fn))
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

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
  :preface
  (defun petmacs/get-project-relateive-dir ()
    "get project relative directory"
    (let ((relative-dir (substring (petmacs--projectile-directory-path) 0 -1)))
      (if (string= relative-dir ".")
          ""
        (concat (format " %s " (nerd-icons-faicon "nf-fa-folder_open")) relative-dir))))
  :ensure nil
  :commands (awesome-tray-update)
  :hook (after-init . awesome-tray-mode)
  :custom-face
  (awesome-tray-module-belong-face ((((background light)) :inherit petmacs-favor-color-face)
                                    (t (:inherit petmacs-favor-color-face))))
  (awesome-tray-module-buffer-name-face ((((background light)) :inherit font-lock-warning-face :bold t)
                                         (t (:inherit font-lock-warning-face :bold t))))
  :init
  (setq awesome-tray-separator " ┃ "
        awesome-tray-hide-mode-line petmacs-disable-modeline
        ;; awesome-tray-mode-line-active-color petmacs-favor-color
        awesome-tray-buffer-name-buffer-changed t
        awesome-tray-buffer-name-max-length 30
        awesome-tray-info-padding-right 1
        awesome-tray-update-interval 0.5
        awesome-tray-belong-update-duration 2.5
        awesome-tray-date-format (concat (format "%s " (nerd-icons-mdicon "nf-md-clock")) "%m-%d %H:%M %a")
        awesome-tray-git-format (concat (format "%s " (nerd-icons-faicon "nf-fa-git_square")) "%s")
        ;; awesome-tray-active-modules   '("pomodoro" "flymake" "pyvenv" "git" "date" )
        ;; awesome-tray-essential-modules '("pomodoro" "date")
        awesome-tray-git-show-status nil)
  (if petmacs-disable-modeline
      (setq awesome-tray-active-modules   '("pomodoro" "buffer-name" "location" "git"  "date")
            awesome-tray-essential-modules '("buffer-name" "location"))
    (setq awesome-tray-active-modules   '("pomodoro" "project-relative-dir" "flymake" "git" "date")
          awesome-tray-essential-modules '("project-relative-dir" "flymake" "date")))
  :config
  (defun awesome-tray-project-relative-dir-info () (format "%s" (petmacs/get-project-relateive-dir)))
  (add-to-list 'awesome-tray-module-alist '("project-relative-dir" . (awesome-tray-project-relative-dir-info awesome-tray-module-parent-dir-face)))

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
        ;; (format "<%s>" pyvenv-workon)
        (concat (format "%s " (nerd-icons-faicon "nf-fae-python")) (format "%s" pyvenv-workon))
      ""))
  (defface awesome-tray-module-pyvenv-face
    '((((background light))
       :foreground "#0673d8" :bold t)
      (t
       :foreground "#369bf7" :bold t))
    "pyvenv face."
    :group 'awesome-tray)
  (add-to-list 'awesome-tray-module-alist '("pyvenv" . (awesome-tray-module-pyvenv-info awesome-tray-module-pyvenv-face)))

  (defun awesome-tray-module-breadcrumbs-info ()
    (breadcrumb-imenu-crumbs))
  (defface awesome-tray-module-breadcrumbs-face
    `((((background light))
       :foreground ,petmacs-favor-color :bold t)
      (t
       :foreground ,petmacs-favor-color :bold t))
    "breadcrumbs face."
    :group 'awesome-tray)
  (add-to-list 'awesome-tray-module-alist '("breadcrumbs" . (awesome-tray-module-breadcrumbs-info awesome-tray-module-breadcrumbs-face)))

  (defun petmacs/awesome-tray-module-flymake-info ()
    "A module for showing Flymake state.(use custom unicode)"
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
                   (waiting (format "%s" (nerd-icons-mdicon "nf-md-timer_sand")))
                   ((null known) (format "%s" (nerd-icons-faicon "nf-fa-question")))
                   (disabledp (format "%s" (nerd-icons-faicon "nf-fa-exclamation")))
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
                                   (propertize (concat (format "%s " (nerd-icons-faicon "nf-fa-info_circle")) (number-to-string .note)) 'face 'awesome-tray-module-flymake-note))
                                 (when (> .warning 0)
                                   (propertize (concat (format "%s " (nerd-icons-faicon "nf-fa-warning")) (number-to-string .warning)) 'face 'awesome-tray-module-flymake-warning))
                                 (when (> .error 0)
                                   (propertize (concat (format "%s " (nerd-icons-faicon "nf-fa-times_circle")) (number-to-string .error)) 'face 'awesome-tray-module-flymake-error)))
                                " "))
                            (propertize (format "%s" (nerd-icons-faicon "nf-fa-check_circle")) 'face 'awesome-tray-green-face)
                            )))))))
              flymake-state)))))
  (advice-add #'awesome-tray-module-flymake-info :override #'petmacs/awesome-tray-module-flymake-info)

  (add-hook 'after-save-hook 'awesome-tray-update))

(unless petmacs-disable-modeline
  (use-package doom-modeline
    :hook (awesome-tray-mode . doom-modeline-mode)
    :init
    (setq doom-modeline-icon petmacs-icon
          doom-modeline-buffer-file-name-style 'buffer-name
          ;; doom-modeline-buffer-file-name-style 'relative-to-project
          ;; disable position percentile
          ;; doom-modeline-percent-position nil
          doom-modeline-support-imenu t
          doom-modeline-minor-modes nil
          doom-modeline-indent-info nil
          doom-modeline-mode-alist nil
          doom-modeline-vcs-max-length 20

          ;; doom-modeline-total-line-number t

          doom-modeline-enable-word-count nil
          doom-modeline-buffer-modification-icon t
          doom-modeline-window-width-limit 110
          doom-modeline-env-version nil)

    ;; Prevent flash of unstyled modeline at startup
    (unless after-init-time
      (setq-default mode-line-format nil))
    :config
    (defun my/update-modeline-box (&rest _)
      (unless (minibufferp)
        (when (eq (window-buffer (selected-window)) (current-buffer))
          (let* ((face (cond
                        ((evil-normal-state-p) 'doom-modeline-evil-normal-state)
                        ((evil-emacs-state-p) 'doom-modeline-evil-emacs-state)
                        ((evil-insert-state-p) 'doom-modeline-evil-insert-state)
                        ((evil-motion-state-p) 'doom-modeline-evil-motion-state)
                        ((evil-visual-state-p) 'doom-modeline-evil-visual-state)
                        ((evil-operator-state-p) 'doom-modeline-evil-operator-state)
                        ((evil-replace-state-p) 'doom-modeline-evil-replace-state)
                        (t 'doom-modeline-evil-user-state)))
                 (color (face-foreground face nil t)))
            (set-face-attribute 'mode-line-active nil :box `(:line-width (-1 . -4) :color ,color))))))
    (add-hook 'post-command-hook #'my/update-modeline-box)

    (doom-modeline-def-segment nyan-position
      "The buffer position information."
      (let ((sep (doom-modeline-spc))
            (wsep (doom-modeline-wspc))
            (face (doom-modeline-face))
            (help-echo "Buffer percentage\n\
mouse-1: Display Line and Column Mode Menu")
            (mouse-face 'doom-modeline-highlight)
            (local-map mode-line-column-line-number-mode-map))
        `(,wsep

          ;; Line and column
          (:propertize
           ((line-number-mode
             (column-number-mode
              (doom-modeline-column-zero-based
               doom-modeline-position-column-line-format
               ,(string-replace
                 "%c" "%C" (car doom-modeline-position-column-line-format)))
              doom-modeline-position-line-format)
             (column-number-mode
              (doom-modeline-column-zero-based
               doom-modeline-position-column-format
               ,(string-replace
                 "%c" "%C" (car doom-modeline-position-column-format)))))
            (doom-modeline-total-line-number
             ,(and doom-modeline-total-line-number
                   (format "/%d" (line-number-at-pos (point-max))))))
           face ,face
           help-echo ,help-echo
           mouse-face ,mouse-face
           local-map ,local-map)

          ((or line-number-mode column-number-mode)
           ,sep)

          ;; must show nyan
          ,(cond
            ((bound-and-true-p nyan-mode)
             (concat sep (nyan-create) sep))
            ((bound-and-true-p poke-line-mode)
             (concat sep (poke-line-create) sep))
            ((bound-and-true-p mlscroll-mode)
             (concat sep
                     (let ((mlscroll-right-align nil))
                       (format-mode-line (mlscroll-mode-line)))
                     sep))
            ((bound-and-true-p sml-modeline-mode)
             (concat sep (sml-modeline-create) sep))
            (t ""))

          ;; Percent position
          (doom-modeline-percent-position
           ((:propertize ("" doom-modeline-percent-position)
             face ,face
             help-echo ,help-echo
             mouse-face ,mouse-face
             local-map ,local-map)
            ,sep)))))

    (doom-modeline-def-segment breadcrumb
      "breadcrumb mode in modeline"
      (if (and (doom-modeline--active) (> (length (breadcrumb-imenu-crumbs)) 0))
          `(,(propertize
              (format " %s %s "
                      (nerd-icons-codicon "nf-cod-triangle_right")
                      (nerd-icons-codicon "nf-cod-symbol_method"))
              'face `(:inherit font-lock-function-name-face :height 1.2)) ,(breadcrumb-imenu-crumbs) " ")
        '("")))

    (doom-modeline-def-modeline
      'petmacs/simple-mode-line
     ;;;; main
      ;; '(eldoc bar workspace-name window-number modals matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
      ;; '(compilation objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker time)

      '(eldoc bar workspace-name window-number matches follow buffer-info remote-host nyan-position word-count parrot selection-info)
      '(compilation objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl input-method indent-info buffer-encoding process))

    ;; Set default mode-line
    (add-hook 'doom-modeline-mode-hook (lambda ()
                                         (doom-modeline-set-modeline 'petmacs/simple-mode-line 'default)))))

(use-package hide-mode-line
  :hook (((treemacs-mode
           eshell-mode shell-mode
           term-mode vterm-mode
           imenu-list-major-mode
           symbols-outline-mode
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
        ;; :hook ((prog-mode
        ;;         conf-mode toml-ts-mode
        ;;         yaml-mode yaml-ts-mode) . display-line-numbers-mode)
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
  :hook (after-init . global-page-break-lines-mode)
  :config (dolist (mode '(dashboard-mode emacs-news-mode))
            (add-to-list 'page-break-lines-modes mode)))

(with-no-warnings
  (when sys/macp
    ;; Render thinner fonts
    (setq ns-use-thin-smoothing t)
    ;; Don't open a file in a new frame
    (setq ns-pop-up-frames nil)))

;; use font supported ligatures
(when (and emacs/>=28p petmacs-enable-ligatures)
  (use-package composite
    :ensure nil
    :init (defvar composition-ligature-table (make-char-table nil))
    :hook (((prog-mode
             conf-mode nxml-mode
             ;; markdown-mode
             help-mode
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

;; disable install emojis when github is unavailable
(use-package emojify
  :hook (after-init . global-emojify-mode)
  :init
  (unless petmacs-quelpa-use-gitee-mirror
    (setq emojify-download-emojis-p t)))

(use-package nyan-mode
  :hook (doom-modeline-mode . nyan-mode)
  :init
  (setq nyan-bar-length 15
        nyan-animate-nyancat t
        nyan-wavy-trail t))

(use-package circadian
  :ensure nil
  :commands (circadian-setup)
  :custom (circadian-themes petmacs-day-night-themes)
  :init (circadian-setup)
  :config
  (when (bound-and-true-p awesome-tray-mode)
    (add-hook circadian-after-load-theme-hook #'awesome-tray-enable)))

;; Frame background transparence
(use-package transwin
  :pretty-hydra
  ((:title (pretty-hydra-title "Frame Management")
    :foreign-keys warn :quit-key ("q" "C-g"))
   ("Actions"
    (("f" make-frame-command "new frame")
     ("d" delete-frame "delete frame")
     ("m" toggle-frame-maximized "maximize" :exit t)
     ("u" toggle-frame-fullscreen "fullscreen" :exit t))
    "opacity"
    (("-" transwin-dec "decrease frame opacity")
     ("=" transwin-inc "increase frame opacity")
     ("0" transwin-toggle "toggle frame opacity")
     ("o" transwin-ask "set frame opacity"))))
  ;; :hook (emacs-startup . (lambda ()
  ;;                          (transwin-ask '95)))
  :bind
  ("M-+" . transwin-inc)
  ("M-_" . transwin-dec)
  ("M-)" . transwin-toggle)
  :init
  (setq transwin-delta-alpha 5
        transwin-parameter-alpha 'alpha-background))

(use-package valign
  :hook ((markdown-mode org-mode) . valign-mode))

(unless petmacs-disable-modeline
  (use-package spacious-padding
    :hook (after-init . spacious-padding-mode)
    :init
    (setq
     spacious-padding-widths
     '( :internal-border-width 8
        :mode-line-width 1
        :right-divider-width 1
        :fringe-width 6
        :tab-width 6
        :tab-bar-width 6
        :tab-line-width 6
        :header-line-width 4
        :scroll-bar-width 8
        ))))

(when emacs/>=26p
  (use-package tab-bar
    :ensure nil
    :bind (("M-[" . tab-bar-switch-to-prev-tab)
           ("M-]" . tab-bar-switch-to-prev-tab)
           ("M-{" . tab-bar-move-tab-backward)
           ("M-}" . tab-bar-move-tab)
           ("M-k" . tab-bar-close-tab))
    ;; :hook (emacs-startup . tab-bar-mode)
    :init
    (setq
     tab-bar-close-button-show nil
     tab-bar-new-tab-choice "*dashboard*";; buffer to show in new tabs
     ;; tab-bar-show petmacs-disable-modeline
     ;; tab-bar-show 1
     tab-bar-show t
     ;; elements to include in bar
     tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
    ;; "Alist of integers to strings of circled unicode numbers."
    tab-bar-tab-hints t)

  ;; (defvar petmacs--circle-numbers-alist
  ;;   '((1 . (nerd-icons-mdicon "nf-md-numeric_1_circle"))
  ;;     (2 . (nerd-icons-mdicon "nf-md-numeric_2_circle"))
  ;;     (3 . (nerd-icons-mdicon "nf-md-numeric_3_circle"))
  ;;     (4 . (nerd-icons-mdicon "nf-md-numeric_4_circle"))
  ;;     (5 . (nerd-icons-mdicon "nf-md-numeric_5_circle"))
  ;;     (6 . (nerd-icons-mdicon "nf-md-numeric_6_circle"))

  ;;     (8 . (nerd-icons-mdicon "nf-md-numeric_8_circle"))
  ;;     (9 . (nerd-icons-mdicon "nf-md-numeric_9_circle"))
  ;;     (10 . (nerd-icons-mdicon "nf-md-numeric_10_circle"))))
  ;; (defun petmacs/tab-bar-tab-name-format-default (tab i)
  ;;   (let ((current-p (eq (car tab) 'current-tab))
  ;;         (tab-num (if (and tab-bar-tab-hints (< i 10))
  ;;                      (alist-get i petmacs--circle-numbers-alist) "")))
  ;;     (propertize
  ;;      (concat tab-num
  ;;              " "
  ;;              (alist-get 'name tab)
  ;;              (or (and tab-bar-close-button-show
  ;;                       (not (eq tab-bar-close-button-show
  ;;                                (if current-p 'non-selected 'selected)))
  ;;                       tab-bar-close-button)
  ;;                  "")
  ;;              " ")
  ;;      'face (funcall tab-bar-tab-face-function tab))))
  ;; (setq tab-bar-tab-name-format-function #'petmacs/tab-bar-tab-name-format-default)
  :config
  (set-face-attribute 'tab-bar-tab nil :background petmacs-favor-color :bold t))

(use-package prettify-utils
  :ensure nil)

(use-package pretty-code
  :after nerd-icons
  :ensure nil
  :commands (pretty-code-add-hook)
  :init
  (pretty-code-add-hook 'python-mode-hook '((:return "return")
                                            (:yield "yield")
                                            ;; (:class "class")
                                            (:raise "raise")
                                            (:lambda "lambda")
                                            (:def "def")))
  (pretty-code-add-hook 'python-ts-mode-hook '((:return "return")
                                               (:yield "yield")
                                               ;; (:class "class")
                                               (:raise "raise")
                                               (:lambda "lambda")
                                               (:def "def")))
  (pretty-code-add-hook 'c-mode-hook     '((:return "return")
                                           ;; (:class "class")
                                           ;; (:struct "struct")
                                           ))
  (pretty-code-add-hook 'c-ts-mode-hook     '((:return "return")
                                              ;; (:class "class")
                                              ;; (:struct "struct")
                                              ))
  (pretty-code-add-hook 'c++-mode-hook     '((:return "return")
                                             ;; (:class "class")
                                             ;; (:struct "struct")
                                             ))
  (pretty-code-add-hook 'c++-ts-mode-hook     '((:return "return")
                                                ;; (:class "class")
                                                ;; (:struct "struct")
                                                ))
  (pretty-code-add-hook 'java-mode-hook     '((:return "return")
                                              ;; (:class "class")
                                              ;; (:struct "struct")
                                              (:throw "throw")))
  (pretty-code-add-hook 'java-ts-mode-hook     '((:return "return")
                                                 ;; (:class "class")
                                                 ;; (:struct "struct")
                                                 (:throw "throw")))
  (pretty-code-add-hook 'emacs-lisp-mode-hook '((:def "defun")
    					                        (:lambda "lambda"))))

(use-package magit-file-icons
  :after magit
  :hook (magit-mode . magit-file-icons-mode)
  :init
  (setq
   magit-file-icons-enable-diff-file-section-icons t
   magit-file-icons-enable-untracked-icons t
   magit-file-icons-enable-diffstat-icons t))

(use-package org-rainbow-tags)

(provide 'init-ui)
