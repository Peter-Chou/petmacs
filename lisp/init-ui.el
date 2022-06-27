;; -*- lexical-binding: t no-byte-compile: t -*-

(require 'init-funcs)

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

(use-package all-the-icons-completion
  :hook ((after-init . all-the-icons-completion-mode)
         (marginalia-mode . all-the-icons-completion-marginalia-setup)))

;; ;; make "unreal" buffers (like popups, sidebars, log buffers,
;; ;; terminals by giving the latter a slightly different (often darker) background
(use-package solaire-mode
  :hook (after-load-theme . solaire-global-mode))

(use-package spacemacs-theme
  :init
  (setq spacemacs-theme-comment-italic t
        spacemacs-theme-org-priority-bold t))

(use-package modus-themes
  :init
  (setq
   modus-themes-bold-constructs t
   modus-themes-italic-constructs t
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

;; (use-package doom-modeline
;;   :preface
;;   (defun petmacs/auto-toggle-pyvenv-mode ()
;;     (if (equal major-mode 'python-mode)
;;         (unless (member '(pyvenv-mode pyvenv-mode-line-indicator) mode-line-misc-info)
;;           (add-to-list 'mode-line-misc-info '(pyvenv-mode pyvenv-mode-line-indicator)))
;;       (when (member '(pyvenv-mode pyvenv-mode-line-indicator) mode-line-misc-info)
;;         (setq mode-line-misc-info (delete '(pyvenv-mode pyvenv-mode-line-indicator)
;;                                           mode-line-misc-info)))
;;       ))
;;   :hook (after-init . doom-modeline-mode)
;;   :init
;;   (setq doom-modeline-icon petmacs-icon
;;         doom-modeline-support-imenu t
;;         doom-modeline-minor-modes nil

;;         ;; doom-modeline-height 1
;;         doom-modeline-height 0.9
;;         doom-modeline-window-width-limit 90
;;         doom-modeline-buffer-file-name-style 'relative-to-project)
;;   ;; Prevent flash of unstyled modeline at startup
;;   (unless after-init-time
;;     (setq-default mode-line-format nil))
;;   :config
;;   (add-hook 'buffer-list-update-hook #'petmacs/auto-toggle-pyvenv-mode))

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
    ;; (if (bound-and-true-p pyvenv-mode)
    (if (and (equal major-mode 'python-mode) (bound-and-true-p pyvenv-virtual-env-name))
        (format "[%s]" pyvenv-virtual-env-name)
      ""))

  (defface awesome-tray-module-pyvenv-face
    '((((background light))
       :foreground "#0673d8" :bold t)
      (t
       :foreground "#369bf7" :bold t))
    "pyvenv face."
    :group 'awesome-tray)
  :commands (awesome-tray-update)
  :hook (after-init . awesome-tray-mode)
  :init
  (setq
   awesome-tray-update-interval 0.6
   awesome-tray-buffer-name-max-length 30
   awesome-tray-file-path-show-filename t
   awesome-tray-buffer-name-buffer-changed t

   ;; awesome-tray-active-modules   '("winum" "location" "belong" "pyvenv" "file-path" "date")
   ;; awesome-tray-essential-modules '("winum" "location" "belong" "file-path")
   awesome-tray-active-modules   '("winum" "location" "belong" "pyvenv" "buffer-name" "date")
   awesome-tray-essential-modules '("winum" "location" "belong" "buffer-name"))
  :config
  (defun petmacs/awesome-tray-update-git-command-cache ()
    (let* ((git-info (awesome-tray-process-exit-code-and-output "git" "symbolic-ref" "--short" "HEAD"))
           (status (nth 0 git-info))
           (result (format "%s" (nth 1 git-info))))
      (setq awesome-tray-git-command-cache
            (if (equal status 0)
                (replace-regexp-in-string "\n" "" result)
              ""))
      awesome-tray-git-command-cache))
  (advice-add #'awesome-tray-update-git-command-cache :override #'petmacs/awesome-tray-update-git-command-cache)

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
  (add-hook 'buffer-list-update-hook #'awesome-tray-update))

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

(use-package all-the-icons
  :if (and petmacs-icon (display-graphic-p))
  :custom (all-the-icons-scale-factor 1.1))

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
      (add-hook 'evil-insert-state-exit-hook #'petmacs/display-line-numbers-relative))
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
(when emacs/>=27p
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

(use-package pretty-code
  :load-path (lambda () (expand-file-name "site-lisp/local/pretty-code" user-emacs-directory))
  :commands (pretty-code-add-hook)
  :init
  (use-package prettify-utils
    :load-path (lambda () (expand-file-name "site-lisp/local/prettify-utils" user-emacs-directory)))

  (pretty-code-add-hook 'python-mode-hook     '((:def "def")
                                                (:class "class")
    					                        (:lambda "lambda")))
  (pretty-code-add-hook 'c-mode-hook     '((:class "class")
                                           (:struct "struct")))
  (pretty-code-add-hook 'c++-mode-hook     '((:class "class")
                                             (:struct "struct")))
  (pretty-code-add-hook 'java-mode-hook     '((:class "class")
    					                      (:lambda "lambda")))
  (pretty-code-add-hook 'scala-mode-hook     '((:def "def")
                                               (:class "class")
                                               (:struct "object")
    					                       (:lambda "lambda")))
  (pretty-code-add-hook 'go-mode-hook     '((:def "func")))
  (pretty-code-add-hook 'emacs-lisp-mode-hook '((:def "defun")
						                        (:lambda "lambda"))))

(use-package kind-all-the-icons
  :load-path (lambda () (expand-file-name "site-lisp/local/kind-all-the-icons" user-emacs-directory))
  :after corfu
  :init
  (when (icon-displayable-p)
    (require 'kind-all-the-icons)
    (if (display-graphic-p)
        (add-to-list 'corfu-margin-formatters #'kind-all-the-icons-margin-formatter))))

(provide 'init-ui)
