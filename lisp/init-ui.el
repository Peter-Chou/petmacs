;; -*- lexical-binding: t no-byte-compile: t -*-

(require 'init-custom)
(require 'init-funcs)

(use-package display-time
  :ensure nil
  :hook (after-init . display-time-mode)
  :init
  (setq display-time-interval 1
        display-time-24hr-format t
        display-time-day-and-date t
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
  (define-key winum-keymap (kbd "M-0") 'winum-select-window-0-or-10))


(use-package nerd-icons-completion
  :when (icons-displayable-p)
  :hook (vertico-mode . nerd-icons-completion-mode))

;; ;; make "unreal" buffers (like popups, sidebars, log buffers,
;; ;; terminals by giving the latter a slightly different (often darker) background
(use-package solaire-mode
  :hook (after-load-theme . solaire-global-mode))

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

(use-package ef-themes)


(use-package everforest
  :quelpa (everforest :fetcher github
  		              :repo "Theory-of-Everything/everforest-emacs"
  		              :files ("*.el")))

(use-package doom-themes
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable customized theme
  ;; FIXME: https://github.com/emacs-lsp/lsp-treemacs/issues/89
  (with-eval-after-load 'lsp-treemacs
    (doom-themes-treemacs-config)))

(if (and (equal petmacs-modeline-style 'awesome-tray)
         (display-graphic-p))
    (use-package awesome-tray
      :quelpa (awesome-tray :fetcher github
  		                    :repo "manateelazycat/awesome-tray"
  		                    :files ("*.el"))
      :preface
      (defun awesome-tray-module-winum-info ()
        (format "%s" (winum-get-number-string)))

      (defface awesome-tray-module-winum-face
        '((((background light))
           :foreground "#0673d7" :bold t)
          (t
           :foreground "#369bf8" :bold t))
        "winum face."
        :group 'awesome-tray)

      (defun awesome-tray-module-pyvenv-info ()
        (if (and (equal major-mode 'python-mode) (bound-and-true-p pyvenv-workon))
            (format "[%s]" pyvenv-workon)
          ""))

      (defface awesome-tray-module-pyvenv-face
        '((((background light))
           :foreground "#0673d8" :bold t)
          (t
           :foreground "#369bf7" :bold t))
        "pyvenv face."
        :group 'awesome-tray)

      (defun awesome-tray-module-pomodoro-info () (format "%s" pomodoro-mode-line-string))

      (defface awesome-tray-module-pomodoro-face
        '((((background light))
           :foreground "#008080" :bold t)
          (t
           :foreground "#00ced1" :bold t))
        "pomodoro face."
        :group 'awesome-tray)

      :commands (awesome-tray-update)
      :hook (after-init . awesome-tray-mode)
      :init
      (setq
       awesome-tray-update-interval 0.6
       awesome-tray-buffer-name-max-length 30
       ;; awesome-tray-mode-line-height 0.15
       awesome-tray-file-path-show-filename t
       awesome-tray-buffer-name-buffer-changed t
       awesome-tray-git-format "·%s·"
       awesome-tray-active-modules   '("anzu" "winum" "location" "pyvenv" "buffer-read-only" "buffer-name" "git" "pomodoro" "date")
       awesome-tray-essential-modules '("winum" "location" "buffer-read-only" "buffer-name"))
      :config
      (setq awesome-tray-module-alist (delq (assoc "buffer-name"  awesome-tray-module-alist) awesome-tray-module-alist))
      ;; use file-path face to show buffer-name info
      (add-to-list 'awesome-tray-module-alist '("buffer-name" . (awesome-tray-module-buffer-name-info awesome-tray-module-file-path-face)))

      (defun petmacs/awesome-tray-module-buffer-name-info ()
        (let (bufname)
          (setq bufname (if awesome-tray-buffer-name-buffer-changed
                            (if (and (buffer-modified-p)
                                     (not (eq buffer-file-name nil)))
                                (concat  awesome-tray-buffer-name-buffer-changed-style (buffer-name))
                              (buffer-name))
                          (format "%s" (buffer-name))))
          (awesome-tray-truncate-string bufname awesome-tray-buffer-name-max-length t)))
      (advice-add #'awesome-tray-module-buffer-name-info :override #'petmacs/awesome-tray-module-buffer-name-info)

      (with-eval-after-load 'modus-themes
        (advice-add #'modus-themes-toggle :after #'awesome-tray-enable))

      (add-to-list 'awesome-tray-module-alist '("winum" . (awesome-tray-module-winum-info awesome-tray-module-winum-face)))
      (add-to-list 'awesome-tray-module-alist '("pyvenv" . (awesome-tray-module-pyvenv-info awesome-tray-module-pyvenv-face)))
      (add-to-list 'awesome-tray-module-alist '("pomodoro" . (awesome-tray-module-pomodoro-info awesome-tray-module-pomodoro-face)))
      (add-hook 'buffer-list-update-hook #'awesome-tray-update))
  (use-package doom-modeline
    :preface
    (defface doom-modeline-python-venv
      '((((background light))
         :foreground "#136207" :bold t)
        (t
         :foreground  "#F37022" :bold t))
      "Face to use for the mode-line python venv."
      :group 'doom-modeline-faces)
    :hook (after-init . doom-modeline-mode)
    :init
    (setq doom-modeline-icon petmacs-icon
          doom-modeline-support-imenu t
          doom-modeline-minor-modes nil
          doom-modeline-indent-info nil
          doom-modeline-height 1
          doom-modeline-window-width-limit 110
          doom-modeline-buffer-file-name-style 'relative-to-project)

    ;; Prevent flash of unstyled modeline at startup
    (unless after-init-time
      (setq-default mode-line-format nil))
    :config
    ;; (doom-modeline-def-segment pomodoro
    ;;                            "pomodoro"
    ;;                            (propertize
    ;;                             (concat
    ;;                              doom-modeline-spc (format "%s" pomodoro-mode-line-string) doom-modeline-spc)
    ;;                             'face (doom-modeline-face 'doom-modeline-urgent)))

    ;; (doom-modeline-def-segment python-venv
    ;;   "python venv"
    ;;   (when (and (doom-modeline--active)
    ;;              (equal major-mode 'python-mode)
    ;;              (bound-and-true-p python-shell-virtualenv-root))
    ;;     (propertize
    ;;      (concat
    ;;       doom-modeline-spc
    ;;       (doom-modeline-icon 'material  "check_circle" "☑" "✔"
    ;;                           :face 'doom-modeline-python-venv
    ;;                           :height 1.3 :v-adjust -0.15)
    ;;       doom-modeline-spc
    ;;       (file-name-nondirectory python-shell-virtualenv-root)
    ;;       doom-modeline-spc)
    ;;      'face (doom-modeline-face 'doom-modeline-python-venv))))

    ;; (defun doom-modeline--check-python-venv-in-modeline ()
    ;;   (member '(pyvenv-mode pyvenv-mode-line-indicator) mode-line-misc-info))

    ;; (defun doom-modeline-override-python-venv-modeline ()
    ;;   "Override default display-time mode-line."
    ;;   (if (and (bound-and-true-p doom-modeline-mode)
    ;;            (bound-and-true-p pyvenv-mode)
    ;;            (doom-modeline--check-python-venv-in-modeline))
    ;;       (setq mode-line-misc-info (delete '(pyvenv-mode pyvenv-mode-line-indicator) mode-line-misc-info))
    ;;     (when (and (bound-and-true-p pyvenv-mode)
    ;;                (not (doom-modeline--check-python-venv-in-modeline)))
    ;;       (add-to-list 'mode-line-misc-info '(pyvenv-mode pyvenv-mode-line-indicator)))))
    ;; (add-hook 'pyvenv-mode-hook #'doom-modeline-override-python-venv-modeline)
    ;; (add-hook 'doom-modeline-mode-hook #'doom-modeline-override-python-venv-modeline)

    ;; (doom-modeline-def-modeline 'petmacs--default-modeline
    ;;   ;; checker is moved from left side of modeline
    ;;   '(bar window-number matches buffer-info remote-host buffer-position parrot selection-info)
    ;;   '(misc-info persp-name github debug repl input-method pomodoro indent-info buffer-encoding process python-venv vcs time))

    ;; Add to `doom-modeline-mode-hook` or other hooks
    ;; (defun petmacs/setup-custom-default-doom-modeline ()
    ;;   (doom-modeline-set-modeline 'petmacs--default-modeline 'default))
    ;; (add-hook 'doom-modeline-mode-hook 'petmacs/setup-custom-default-doom-modeline)
    ))

(use-package hide-mode-line
  :hook (((
           completion-list-mode
           eshell-mode shell-mode
           term-mode vterm-mode
           lsp-ui-imenu-mode
           pdf-annot-list-mode) . hide-mode-line-mode)))

;; Show native line numbers if possible, otherwise use `linum'
(when petmacs-enable-display-line-numbers
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

;; Child frame
(when (childframe-workable-p)
  (use-package posframe
    :hook (after-load-theme . posframe-delete-all)
    :init
    (defface posframe-border
      `((t (:inherit region)))
      "Face used by the `posframe' border."
      :group 'posframe)
    (defvar posframe-border-width 2
      "Default posframe border width.")
    :config
    (with-no-warnings
      (defun my-posframe--prettify-frame (&rest _)
        (set-face-background 'fringe nil posframe--frame))
      (advice-add #'posframe--create-posframe :after #'my-posframe--prettify-frame)

      (defun posframe-poshandler-frame-center-near-bottom (info)
        (cons (/ (- (plist-get info :parent-frame-width)
                    (plist-get info :posframe-width))
                 2)
              (/ (+ (plist-get info :parent-frame-height)
                    (* 2 (plist-get info :font-height)))
                 2))))))


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
    ;; M-x kind-icon-preview-all to reset and preview all icons after installation
    (use-package kind-icon
      :quelpa (kind-icon :fetcher github
  		                 :repo "jdtsmith/kind-icon"
  		                 :files ("*.el"))
      :after corfu
      :init
      (require 'kind-icon)
      ;; to compute blended backgrounds correctly
      (setq ;; kind-icon-default-face 'corfu-default
       kind-icon-use-icons nil
       kind-icon-mapping
       `(
         (array ,(nerd-icons-codicon "nf-cod-symbol_array") :face font-lock-type-face)
         (boolean ,(nerd-icons-codicon "nf-cod-symbol_boolean") :face font-lock-builtin-face)
         (class ,(nerd-icons-codicon "nf-cod-symbol_class") :face font-lock-type-face)
         (color ,(nerd-icons-codicon "nf-cod-symbol_color") :face success)
         (command ,(nerd-icons-codicon "nf-cod-terminal") :face default)
         (constant ,(nerd-icons-codicon "nf-cod-symbol_constant") :face font-lock-constant-face)
         (constructor ,(nerd-icons-codicon "nf-cod-triangle_right") :face font-lock-function-name-face)
         (enummember ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
         (enum-member ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
         (enum ,(nerd-icons-codicon "nf-cod-symbol_enum") :face font-lock-builtin-face)
         (event ,(nerd-icons-codicon "nf-cod-symbol_event") :face font-lock-warning-face)
         (field ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-variable-name-face)
         (file ,(nerd-icons-codicon "nf-cod-symbol_file") :face font-lock-string-face)
         (folder ,(nerd-icons-codicon "nf-cod-folder") :face font-lock-doc-face)
         (interface ,(nerd-icons-codicon "nf-cod-symbol_interface") :face font-lock-type-face)
         (keyword ,(nerd-icons-codicon "nf-cod-symbol_keyword") :face font-lock-keyword-face)
         (macro ,(nerd-icons-codicon "nf-cod-symbol_misc") :face font-lock-keyword-face)
         (magic ,(nerd-icons-codicon "nf-cod-wand") :face font-lock-builtin-face)
         (method ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
         (function ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
         (module ,(nerd-icons-codicon "nf-cod-file_submodule") :face font-lock-preprocessor-face)
         (numeric ,(nerd-icons-codicon "nf-cod-symbol_numeric") :face font-lock-builtin-face)
         (operator ,(nerd-icons-codicon "nf-cod-symbol_operator") :face font-lock-comment-delimiter-face)
         (param ,(nerd-icons-codicon "nf-cod-symbol_parameter") :face default)
         (property ,(nerd-icons-codicon "nf-cod-symbol_property") :face font-lock-variable-name-face)
         (reference ,(nerd-icons-codicon "nf-cod-references") :face font-lock-variable-name-face)
         (snippet ,(nerd-icons-codicon "nf-cod-symbol_snippet") :face font-lock-string-face)
         (string ,(nerd-icons-codicon "nf-cod-symbol_string") :face font-lock-string-face)
         (struct ,(nerd-icons-codicon "nf-cod-symbol_structure") :face font-lock-variable-name-face)
         (text ,(nerd-icons-codicon "nf-cod-text_size") :face font-lock-doc-face)
         (typeparameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
         (type-parameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
         (unit ,(nerd-icons-codicon "nf-cod-symbol_ruler") :face font-lock-constant-face)
         (value ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-builtin-face)
         (variable ,(nerd-icons-codicon "nf-cod-symbol_variable") :face font-lock-variable-name-face)
         (t ,(nerd-icons-codicon "nf-cod-code") :face font-lock-warning-face)))
      :config
      (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

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

;;; load theme
;; (petmacs--load-theme petmacs-default-theme)
                                        ;
(use-package circadian
  :load-path (lambda () (expand-file-name "site-lisp/local/circadian" user-emacs-directory))
  :commands (circadian-setup)
  :custom (circadian-themes petmacs-day-night-themes)
  :init (circadian-setup)
  :config
  (when (bound-and-true-p awesome-tray-mode)
    (add-hook circadian-after-load-theme-hook #'awesome-tray-enable)))

(provide 'init-ui)
