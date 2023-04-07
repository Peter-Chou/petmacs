;; -*- lexical-binding: t no-byte-compile: t -*-

(require 'init-custom)
(require 'init-const)
(require 'init-funcs)

(use-package which-key
  :diminish
  :hook (after-init . which-key-mode)
  :init
  (setq which-key-idle-delay 0.2)
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix " ")

  ;; Needed to avoid nil variable error before update to recent which-key
  (defvar which-key-replacement-alist nil)
  ;; Reset to the default or customized value before adding our values in order
  ;; to make this initialization code idempotent.
  (custom-reevaluate-setting 'which-key-replacement-alist)
  ;; Replace rules for better naming of functions
  (let ((new-descriptions
         ;; being higher in this list means the replacement is applied later
         '(
           ("petmacs/\\(.+\\)" . "\\1")
           ("petmacs/toggle-\\(.+\\)" . "\\1")
           ("avy-goto-word-or-subword-1" . "avy word")
           ("shell-command" . "shell cmd")
           ("universal-argument" . "universal arg")
           ("er/expand-region" . "expand region")
           ("evil-lisp-state-\\(.+\\)" . "\\1")
           )))
    (dolist (nd new-descriptions)
      ;; ensure the target matches the whole string
      (push (cons (cons nil (concat "\\`" (car nd) "\\'")) (cons nil (cdr nd)))
            which-key-replacement-alist))))

;; Jump to things in Emacs tree-style
(use-package avy
  :hook (after-init . avy-setup-default)
  :config (setq avy-all-windows nil
                avy-all-windows-alt t
                avy-background t
                avy-style 'pre))

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :diminish
  :hook ((after-init . global-aggressive-indent-mode)
         ;; NOTE: Disable in large files due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         (find-file . (lambda ()
                        (when (too-long-file-p)
                          (aggressive-indent-mode -1)))))
  :config
  ;; Disable in some modes
  (dolist (mode '(gitconfig-mode asm-mode web-mode html-mode css-mode go-mode scala-mode prolog-inferior-mode))
    (push mode aggressive-indent-excluded-modes))

  ;; Disable in some commands
  (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t)

  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list 'aggressive-indent-dont-indent-if
               '(and (derived-mode-p 'c-mode 'c++-mode 'csharp-mode
                                     'java-mode 'go-mode 'swift-mode)
                     (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                                         (thing-at-point 'line))))))

;; Show number of matches in mode-line while searching
(use-package anzu
  :diminish
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook (after-init . global-anzu-mode))

;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :bind ([remap comment-dwim] . comment-dwim-2))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-merge-split-window-function 'split-window-horizontally))

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(use-package expand-region)

;; Hungry deletion
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :init (setq hungry-delete-chars-to-skip " \t\f\v"
              hungry-delete-except-modes
              '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode)))

;; Drag stuff (lines, words, region, etc...) around
(use-package drag-stuff
  :diminish
  :hook (after-init . drag-stuff-global-mode)
  :config
  (evil-define-key nil evil-motion-state-map (kbd "M-n") 'drag-stuff-down)
  (evil-define-key nil evil-motion-state-map (kbd "M-p") 'drag-stuff-up)
  (evil-define-key nil evil-motion-state-map (kbd "M-h") 'drag-stuff-left)
  (evil-define-key nil evil-motion-state-map (kbd "M-l") 'drag-stuff-right)

  (add-to-list 'drag-stuff-except-modes 'org-mode))

;; Move to the beginning/end of line or code
(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line] . mwim-end-of-code-or-line)))

;; Windows-scroll commands
(use-package pager
  :bind (([remap scroll-up-command] . pager-page-down)
         ([remap scroll-down-command] . pager-page-up)
         ([next]   . pager-page-down)
         ([prior]  . pager-page-up)
         ([M-up]   . pager-row-up)
         ([M-kp-8] . pager-row-up)
         ([M-down] . pager-row-down)
         ([M-kp-2] . pager-row-down)))

;; Preview when `goto-char'
(use-package goto-char-preview
  :bind ([remap goto-char] . goto-char-preview))

;; Preview when `goto-line'
(use-package goto-line-preview
  :bind ([remap goto-line] . goto-line-preview))

;; Handling capitalized subwords in a nomenclature
(use-package subword
  :ensure nil
  :diminish
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

;; Open files as another user
(use-package sudo-edit
  :init (setq sudo-edit-user "root"))

;; Flexible text folding
(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :config
  ;; More functions
  ;; @see https://karthinks.com/software/simple-folding-with-hideshow/
  (defun hs-cycle (&optional level)
    (interactive "p")
    (let (message-log-max
          (inhibit-message t))
      (if (= level 1)
          (pcase last-command
            ('hs-cycle
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))
            ('hs-cycle-children
             (save-excursion (hs-show-block))
             (setq this-command 'hs-cycle-subtree))
            ('hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
        (hs-hide-level level)
        (setq this-command 'hs-hide-level))))

  (defun hs-toggle-all ()
    "Toggle hide/show all."
    (interactive)
    (pcase last-command
      ('hs-toggle-all
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))

  ;; Display line counts
  (defun hs-display-code-line-counts (ov)
    "Display line counts when hiding codes."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (concat
                    " "
                    (propertize
                     (if (char-displayable-p ?⏷) "⏷" "...")
                     'face 'shadow)
                    (propertize
                     (format " (%d lines)"
                             (count-lines (overlay-start ov)
                                          (overlay-end ov)))
                     'face '(:inherit shadow :height 0.8))
                    " "))))
  (setq hs-set-up-overlay #'hs-display-code-line-counts))

(use-package posframe)
(use-package general)
(use-package bind-map)
(use-package bind-key)

;; Easily adjust the font size in all frames
(use-package default-text-scale
  :hook (after-init . default-text-scale-mode))

(use-package pretty-hydra
  :init
  (require 'pretty-hydra)
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:foreground ,(face-background 'highlight))))
          (height (or height 1.0))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (icon-displayable-p) icon-type icon-name)
         (let ((f (intern (format "all-the-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face)))))

(use-package pomodoro
  :init
  (require 'pomodoro)
  (defun petmacs/pomodoro-toggle ()
    (interactive)
    (if (string= "" pomodoro-mode-line-string)
        (pomodoro-start pomodoro-work-time)
      (pomodoro-stop)))
  (setq
   pomodoro-break-start-sound (expand-file-name "data/sounds/emacs.d_sounds_three_beeps.wav" user-emacs-directory)
   pomodoro-work-start-sound (expand-file-name "data/sounds/emacs.d_sounds_jabber_message.wav" user-emacs-directory)))

(use-package visual-regexp
  :defer
  :commands (vr/replace vr/query-replace))


(use-package visual-regexp-steroids
  :defer
  :commands (vr/select-replace vr/select-query-replace))

(use-package protobuf-mode
  :hook (protobuf-mode . disable-curly-bracket-electric-pair)
  :config
  (define-key protobuf-mode-map (kbd "RET") 'av/auto-indent-method-maybe))

(use-package writeroom-mode
  ;; :hook ((prog-mode yaml-mode markdown-mode org-mode) . writeroom-mode)
  :init (setq writeroom-mode-line t
              writeroom-maximize-window nil
              writeroom-fullscreen-effect 'maximized
              writeroom-width 90)
  :config
  (with-eval-after-load 'writeroom-mode
    (define-key writeroom-mode-map (kbd "C-M-<") #'writeroom-decrease-width)
    (define-key writeroom-mode-map (kbd "C-M->") #'writeroom-increase-width)
    (define-key writeroom-mode-map (kbd "C-M-=") #'writeroom-adjust-width)))

(use-package editorconfig
  :diminish
  :hook (after-init . editorconfig-mode))

(use-package toggle-one-window
  :quelpa
  (toggle-one-window :fetcher github
  		             :repo "manateelazycat/toggle-one-window"
  		             :files ("*.el"))
  :commands (toggle-one-window))

;; Search tool
(use-package grep
  :ensure nil
  :commands grep-apply-setting
  :config
  (when (executable-find "rg")
    (grep-apply-setting
     'grep-command "rg --color=auto --null -nH --no-heading -e ")
    (grep-apply-setting
     'grep-template "rg --color=auto --null --no-heading -g '!*/' -e <R> <D>")
    (grep-apply-setting
     'grep-find-command '("rg --color=auto --null -nH --no-heading -e ''" . 38))
    (grep-apply-setting
     'grep-find-template "rg --color=auto --null -nH --no-heading -e <R> <D>")))

(use-package rime
  :init
  (setq default-input-method "rime"
        rime-title (char-to-string 20013) ;; "中"
        rime-cursor "˰"
        rime-user-data-dir (expand-file-name "data/rime" user-emacs-directory)
        rime-show-candidate (petmacs/candidate-show-framework)

        ;; rime-librime-root (expand-file-name "librime/build" user-emacs-directory)
        ;; rime-emacs-module-header-root "/home/peter/emacs-28.1-native-comp/src"

        rime-disable-predicates
        '(rime-predicate-evil-mode-p ;; 在 evil-mode 的非编辑状态下
          ;; rime-predicate-after-alphabet-char-p ;; 在英文字符串之后（必须为以字母开头的英文字符串）
          ;; rime-predicate-punctuation-line-begin-p ;; 在行首要输入符号时
          ;; rime-predicate-current-uppercase-letter-p ;; 将要输入的为大写字母时
          ;; rime-predicate-tex-math-or-command-p ;; 在 (La)TeX 数学环境中或者输入 (La)TeX 命令时
          rime-predicate-prog-in-code-p ;; 在 prog-mode 和 conf-mode 中除了注释和引号内字符串之外的区域
          rime-predicate-ace-window-p ;; 激活 ace-window-mode
          rime-predicate-hydra-p ;; 激活了一个 hydra keymap
          )

        rime-posframe-properties (list :internal-border-width 1))
  :config
  (set-face-attribute 'rime-highlight-candidate-face nil :foreground petmacs-favor-color :bold t)
  (set-face-attribute 'rime-code-face nil :foreground petmacs-favor-color :bold t)

  (define-key rime-mode-map (kbd "M-j") 'rime-force-enable))

;; add space between Chinese and English character
;; these white-space characters are not really added to the contents, it just like to do.
(use-package pangu-spacing
  :init
  (global-pangu-spacing-mode 1)
  :config
  ;; Always insert `real' space in org-mode.
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local pangu-spacing-real-insert-separtor t))))

(defconst tree-sitter--fold-supported-major-mode-hooks
  '(
    sh-mode-hook
    c-mode-hook
    c++-mode-hook
    csharp-mode-hook
    css-mode-hook
    ess-r-mode-hook
    go-mode-hook
    html-mode-hook
    java-mode-hook
    javascript-mode-hook
    js-mode-hook
    js2-mode-hook
    js3-mode-hook
    json-mode-hook
    jsonc-mode-hook
    nix-mode-hook
    php-mode-hook
    python-mode-hook
    rjsx-mode-hook
    ruby-mode-hook
    rust-mode-hook
    rustic-mode-hook
    scala-mode-hook
    swift-mode-hook
    typescript-mode-hook))

;; Tree-sitter: need dynamic module feature
;; (when (functionp 'module-load)
;;   (use-package tree-sitter
;;     :diminish
;;     :hook ((after-init . global-tree-sitter-mode)
;;            (tree-sitter-after-on . tree-sitter-hl-mode))))

;; (use-package tree-sitter-langs)

;; (use-package tree-sitter-indent
;;   :hook (rust-mode . tree-sitter-indent-mode))

;; (use-package ts-fold
;;   :quelpa (ts-fold :fetcher github
;;   		           :repo "jcs090218/ts-fold"
;;   		           :files ("*.el"))
;;   :pretty-hydra
;;   ((:foreign-keys warn :quit-key "q")
;;    ("Toggle"
;;     (("t" ts-fold-toggle "toggle at point" :exit t))
;;     "Open"
;;     (
;;      ("o" ts-fold-open "open at point" :exit t)
;;      ("r" ts-fold-open-all "open all" :exit t)
;;      ("O" ts-fold-open-recursively "recursive open at point" :exit t))
;;     "Close"
;;     (("c" ts-fold-close "close at point" :exit t)
;;      ("m" ts-fold-close-all "close all" :exit t))))
;;   :init
;;   (setq ts-fold-indicators-fringe 'right-fringe
;;         ;; don't obscure lint and breakpoint indicators
;;         ts-fold-indicators-priority 0
;;         )
;;   (dolist (mode-hook tree-sitter--fold-supported-major-mode-hooks)
;;     (when (boundp mode-hook)
;;       (add-hook mode-hook #'ts-fold-mode)
;;       (add-hook mode-hook #'ts-fold-indicators-mode))))

(use-package list-environment
  :hook (list-environment-mode . (lambda ()
                                   (setq tabulated-list-format
                                         (vconcat `(("" ,(if (icon-displayable-p) 2 0)))
                                                  tabulated-list-format))
                                   (tabulated-list-init-header)))
  :init
  (with-no-warnings
    (defun my-list-environment-entries ()
      "Generate environment variable entries list for tabulated-list."
      (mapcar (lambda (env)
                (let* ((kv (split-string env "="))
                       (key (car kv))
                       (val (mapconcat #'identity (cdr kv) "=")))
                  (list key (vector
                             (if (icon-displayable-p)
                                 (all-the-icons-octicon "key" :height 0.8 :v-adjust -0.05)
                               "")
                             `(,key face font-lock-keyword-face)
                             `(,val face font-lock-string-face)))))
              process-environment))
    (advice-add #'list-environment-entries :override #'my-list-environment-entries)))


(use-package watch-other-window
  :quelpa (watch-other-window :fetcher github
  		                      :repo "manateelazycat/watch-other-window"
  		                      :files ("*.el"))
  :init
  (require 'watch-other-window)
  (define-key evil-motion-state-map (kbd "C-j") #'watch-other-window-up-line)
  (define-key evil-motion-state-map (kbd "C-k") #'watch-other-window-down-line)
  (define-key evil-motion-state-map (kbd "M-j") #'watch-other-window-up)
  (define-key evil-motion-state-map (kbd "M-k") #'watch-other-window-down))

(use-package quickrun
  :commands (quickrun)
  :init
  (quickrun-add-command "c++/c1z"
    '((:command . "g++")
      (:exec . ("%c -std=c++1z %o -o %e %s"
                "%e %a"))
      (:remove . ("%e")))
    :default "c++")
  (quickrun-add-command "python"
    '((:command . "python3"))
    :default "python")
  :config
  (define-key quickrun--mode-map (kbd "C-c C-k") 'quickrun--kill-running-process))

(use-package uuidgen
  :commands (uuidgen))

(use-package centered-cursor-mode)
(use-package restart-emacs)
(use-package focus)                     ; Focus on the current region
(use-package disk-usage)                     ; Focus on the current region
(use-package imenu-list)
(use-package iedit)
(use-package dotenv-mode)

(unless sys/win32p
  (use-package daemons)                 ; system services/daemons
  (use-package tldr))

;; Visual `align-regexp'
(use-package ialign)

(provide 'init-tools)
