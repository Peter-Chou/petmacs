;r; -*- lexical-binding: t no-byte-compile: t -*-

(require 'init-custom)
(require 'init-const)
(require 'init-funcs)

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

(use-package posframe-plus
  :ensure nil
  :init (require 'posframe-plus))

;; F5 for paging which-key
(use-package which-key
  :diminish
  :hook (after-init . which-key-mode)
  :init
  (setq which-key-idle-delay 0.2
        which-key-add-column-padding 1
        which-key-allow-multiple-replacements t
        which-key-echo-keystrokes 0.02
        which-key-idle-secondary-delay 0.01
        which-key-max-display-columns nil
        which-key-sort-order 'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-special-keys nil
        which-key-allow-evil-operators t
        which-key-separator " "
        which-key-prefix-prefix " "
        which-key-lighter nil
        which-key-show-remaining-keys t
        which-key-max-description-length 30
        )

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

(when (childframe-completion-workable-p)
  (use-package which-key-posframe
    :diminish
    :functions posframe-poshandler-frame-center-near-bottom
    :custom-face
    (which-key-posframe ((t (:inherit tooltip))))
    (which-key-posframe-border ((t (:inherit posframe-border :background unspecified))))
    :init
    (setq which-key-posframe-border-width posframe-border-width
          which-key-posframe-poshandler #'posframe-poshandler-frame-center-near-bottom
          which-key-posframe-parameters '((left-fringe . 8)
                                          (right-fringe . 8)))
    (which-key-posframe-mode 1)))

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
  (dolist (mode '(gitconfig-mode
                  protobuf-mode
                  asm-mode web-mode html-mode
                  css-mode css-ts-mode
                  python-mode python-ts-mode
                  go-mode go-ts-mode
                  java-mode java-ts-mode
                  shell-mode term-mode vterm-mode
                  scala-mode prolog-inferior-mode))
    (push mode aggressive-indent-excluded-modes))

  ;; Disable in some commands
  (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t)

  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list 'aggressive-indent-dont-indent-if
               '(and (derived-mode-p 'c-mode c-ts-mode 'c-or-c++-ts-mode 'c++-mode 'c++-ts-mode
                                     'csharp-mode 'csharp-ts-mode 'java-mode 'java-ts-mode
                                     'go-mode go-ts-mode 'swift-mode)
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

(use-package expand-region
  :config
  (when (petmacs-treesit-available-p)
    (defun treesit-mark-bigger-node ()
      "Use tree-sitter to mark regions."
      (let* ((root (treesit-buffer-root-node))
             (node (treesit-node-descendant-for-range root (region-beginning) (region-end)))
             (node-start (treesit-node-start node))
             (node-end (treesit-node-end node)))
        ;; Node fits the region exactly. Try its parent node instead.
        (when (and (= (region-beginning) node-start) (= (region-end) node-end))
          (when-let ((node (treesit-node-parent node)))
            (setq node-start (treesit-node-start node)
                  node-end (treesit-node-end node))))
        (set-mark node-end)
        (goto-char node-start)))
    (add-to-list 'er/try-expand-list 'treesit-mark-bigger-node)))

;; Hungry deletion
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :init (setq hungry-delete-chars-to-skip " \t\f\v"
              hungry-delete-except-modes
              '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode)))

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
    (let ((face (or face `(:inherit highlight :reverse-video t)))
          (height (or height 1.2))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (icons-displayable-p) icon-type icon-name)
         (let ((f (intern (format "nerd-icons-%s" icon-type))))
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
   pomodoro-work-time 25
   pomodoro-break-time 7
   pomodoro-extra-time 2
   pomodoro-play-sounds t
   pomodoro-work-cycle "work "
   pomodoro-break-cycle "rest "
   pomodoro-break-start-sound (expand-file-name "data/sounds/emacs.d_sounds_three_beeps.wav" user-emacs-directory)
   pomodoro-work-start-sound (expand-file-name "data/sounds/emacs.d_sounds_jabber_message.wav" user-emacs-directory)))

(use-package visual-regexp
  :defer
  :commands (vr/replace vr/query-replace))


(use-package visual-regexp-steroids
  :defer
  :commands (vr/select-replace vr/select-query-replace))

(use-package protobuf-mode
  :mode ("\\.pbtxt\\'" . protobuf-mode))

(use-package writeroom-mode
  :pretty-hydra
  ((:title (pretty-hydra-title "writeroom Management")
    :foreign-keys warn :quit-key ("q" "C-g"))
   ("Actions"
    (("m" writeroom-toggle-mode-line "modeline")
     ("[" writeroom-decrease-width   "shrink")
     ("]" writeroom-increase-width   "enlarge")
     ("=" writeroom-adjust-width     "adjust width" :exit t))))
  ;; :hook ((prog-mode yaml-mode markdown-mode org-mode) . writeroom-mode)
  :init (setq writeroom-mode-line t
              writeroom-maximize-window nil
              writeroom-fullscreen-effect 'maximized
              writeroom-width 90)
  (require 'writeroom-mode)
  ;; :config
  ;; (with-eval-after-load 'writeroom-mode
  ;;   (define-key writeroom-mode-map (kbd "C-M-<") #'writeroom-decrease-width)
  ;;   (define-key writeroom-mode-map (kbd "C-M->") #'writeroom-increase-width)
  ;;   (define-key writeroom-mode-map (kbd "C-M-=") #'writeroom-adjust-width))
  )

   (use-package editorconfig
     :diminish
     :hook (after-init . editorconfig-mode))

;; Search tool
(use-package grep
  :ensure nil
  :autoload grep-apply-setting
  :init
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

(use-package list-environment
  :init
  (with-no-warnings
    (defun my-list-environment-entries ()
      "Generate environment variable entries list for tabulated-list."
      (mapcar (lambda (env)
                (let* ((kv (split-string env "="))
                       (key (car kv))
                       (val (mapconcat #'identity (cdr kv) "=")))
                  (list key (vector
                             `(,key face font-lock-keyword-face)
                             `(,val face font-lock-string-face)))))
              process-environment))
    (advice-add #'list-environment-entries :override #'my-list-environment-entries)))

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

(use-package symbols-outline
  :after nerd-icons
  :preface
  (defun petmacs/symbols-outline-smart-toggle ()
    "Toggle `symbols-outline-mode' by showing or quitting the `*Outline*' buffer."
    (interactive)
    (if (get-buffer-window symbols-outline-buffer-name t)
        (progn
          (ignore-errors (quit-windows-on symbols-outline-buffer-name))
          (when (get-buffer symbols-outline-buffer-name)
            (bury-buffer (get-buffer symbols-outline-buffer-name))))
      (symbols-outline-show)))
  :init
  (require 'symbols-outline)
  (setq symbols-outline-window-position 'left
        symbols-outline-use-nerd-icon-in-gui (not (image-type-available-p 'svg))
        symbols-outline-window-width 35
        ;; symbols-outline-ignore-variable-symbols nil
        symbols-outline-buffer-name "*Outline*"
        symbols-outline-collapse-functions-on-startup t)

  (when (member petmacs-lsp-mode-impl '(lsp-mode eglot-mode))
    (setq symbols-outline-fetch-fn #'symbols-outline-lsp-fetch))
  :config
  (require 'nerd-icons)

  (evil-define-key 'normal symbols-outline-mode-map
    (kbd "g") 'symbols-outline-refresh
    (kbd "n") 'symbols-outline-next
    (kbd "p") 'symbols-outline-prev
    (kbd "f") 'symbols-outline-next-same-level
    (kbd "b") 'symbols-outline-prev-same-level
    (kbd "u") 'symbols-outline-move-depth-up
    (kbd "d") 'symbols-outline-move-depth-down
    (kbd "TAB") 'symbols-outline-toggle-node
    [tab] 'symbols-outline-toggle-node
    (kbd "S-TAB") 'symbols-outline-cycle-visibility-globally
    [backtab] 'symbols-outline-cycle-visibility-globally
    (kbd "RET") 'symbols-outline-visit
    (kbd "M-RET") 'symbols-outline-visit-and-quit)
  (symbols-outline-follow-mode))

(use-package iedit
  :init
  (setq iedit-toggle-key-default nil)
  :config
  (define-key iedit-mode-keymap (kbd "M-h") 'iedit-restrict-function)
  (define-key iedit-mode-keymap (kbd "M-i") 'iedit-restrict-current-line))

(use-package imenu-list
  :init (setq imenu-list-size 35))

(use-package docker)

(use-package centered-cursor-mode)
(use-package restart-emacs)
(use-package focus)                     ; Focus on the current region
(use-package disk-usage)                     ; Focus on the current region
(use-package rg)
(use-package dotenv-mode)

(unless sys/win32p
  (use-package daemons)                 ; system services/daemons
  (use-package tldr))

;; Visual `align-regexp'
(use-package ialign)

(use-package golden-ratio)

;; elastic search
(use-package es-mode
  :mode ("\\.es\\'" . es-mode))

(use-package smart-semicolon
  :hook (((c-mode-common java-ts-mode) . smart-semicolon-mode)))

(provide 'init-tools)
