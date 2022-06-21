;; init-tools.el --- Better default configurations.	-*- lexical-binding: t -*-

(use-package posframe)

(use-package general)
(use-package bind-map)
(use-package bind-key)

(use-package pretty-hydra
  :init (require 'pretty-hydra))

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

(use-package display-fill-column-indicator
  :ensure nil
  :hook
  (prog-mode . display-fill-column-indicator-mode)
  :init
  (setq-default fill-column  80)
  (setq display-fill-column-indicator-character "|"))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package visual-regexp
  :defer
  :commands (vr/replace vr/query-replace))


(use-package visual-regexp-steroids
  :defer
  :commands (vr/select-replace vr/select-query-replace))


(use-package org-super-agenda)

(use-package protobuf-mode
  :hook (protobuf-mode . disable-curly-bracket-electric-pair))

(use-package olivetti
  :init
  (setq olivetti-body-width nil)
  :config
  (defun distraction-free ()
    "Distraction-free writing environment"
    (interactive)
    (if (equal olivetti-mode nil)
        (olivetti-mode t)
      (progn
        (olivetti-mode 0)))))

(use-package editorconfig
  :diminish
  :hook (after-init . editorconfig-mode))

(use-package toggle-one-window
  :quelpa
  (toggle-one-window :fetcher github
  		             :repo "manateelazycat/toggle-one-window"
  		             :files ("*.el"))
  :commands (toggle-one-window))

(use-package ace-window
  :pretty-hydra
  ((:foreign-keys warn :quit-key "q")
   ("Actions"
    (("TAB" other-window "switch")
     ("x" ace-delete-window "delete" :exit t)
     ("X" ace-delete-other-windows "delete other" :exit t)
     ("s" ace-swap-window "swap" :exit t)
     ("a" ace-select-window "select" :exit t)
     ("m" toggle-frame-maximized "maximize" :exit t)
     ("f" toggle-frame-fullscreen "fullscreen" :exit t))
    "Resize"
    (("h" shrink-window-horizontally "←")
     ("j" enlarge-window "↓")
     ("k" shrink-window "↑")
     ("l" enlarge-window-horizontally "→")
     ("n" balance-windows "balance" :exit t))
    "Split"
    (("r" split-window-right "horizontally")
     ("R" split-window-horizontally-instead "horizontally instead")
     ("v" split-window-below "vertically")
     ("V" split-window-vertically-instead "vertically instead")
     ("t" toggle-window-split "toggle"))
    "Zoom"
    (("+" text-scale-increase "in")
     ("=" text-scale-increase "in")
     ("-" text-scale-decrease "out")
     ("0" (text-scale-increase 0) "reset"))
    "Appearance"
    (("F" set-frame-font "font")
     ("T" consult-theme "theme"))))
  :custom-face
  (aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 2.0))))
  (aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 1.0))))
  (aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
  :bind ([remap other-window] . ace-window)
  :init
  ;; (setq aw-scope 'frame) ;; jump only in current frame
  (setq aw-minibuffer-flag t))

;; Enforce rules for popups
(use-package popper
  :defines popper-echo-dispatch-actions
  :commands popper-group-by-projectile
  :hook (emacs-startup . popper-mode)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$" "\\*Pp Eval Output\\*$"
          "\\*Compile-Log\\*"
          "\\*Completions\\*"
          "\\*Warnings\\*"
          "\\*Async Shell Command\\*"
          "\\*Apropos\\*"
          "\\*Backtrace\\*"
          "\\*Calendar\\*"
          "\\*Embark Actions\\*"
          "\\*Finder\\*"
          "\\*Kill Ring\\*"

          "\\*lsp-bridge-ref\\*"

          bookmark-bmenu-mode
          comint-mode
          compilation-mode
          help-mode helpful-mode
          tabulated-list-mode
          Buffer-menu-mode

          grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
          process-menu-mode list-environment-mode cargo-process-mode

          "^\\*eshell.*\\*.*$" eshell-mode
          "^\\*shell.*\\*.*$"  shell-mode
          "^\\*terminal.*\\*.*$" term-mode
          "^\\*vterm.*\\*.*$"  vterm-mode

          "\\*DAP Templates\\*$" dap-server-log-mode
          "\\*ELP Profiling Restuls\\*" profiler-report-mode
          "\\*Flycheck errors\\*$" " \\*Flycheck checker\\*$"
          "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
          "\\*[Wo]*Man.*\\*$"
          "\\*ert\\*$" overseer-buffer-mode
          "\\*gud-debug\\*$"
          "\\*lsp-help\\*$" "\\*lsp session\\*$"
          "\\*quickrun\\*$"
          "\\*tldr\\*$"
          "\\*vc-.*\\*$"
          "^\\*elfeed-entry\\*$"
          "^\\*macro expansion\\**"

          "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
          "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
          "\\*docker-containers\\*" "\\*docker-images\\*" "\\*docker-networks\\*" "\\*docker-volumes\\*"
          "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
          "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
          rustic-cargo-outdated-mode rustic-cargo-test-moed))
  (with-eval-after-load 'projectile
    (setq popper-group-function #'popper-group-by-projectile))
  (setq popper-echo-dispatch-actions t)
  :config
  (popper-echo-mode 1)

  (with-no-warnings
    (defun petmacs/popper-fit-window-height (win)
      "Determine the height of popup window WIN by fitting it to the buffer's content."
      (fit-window-to-buffer
       win
       (floor (frame-height) 3)
       (floor (frame-height) 3)))
    (setq popper-window-height #'petmacs/popper-fit-window-height)

    (defun popper-close-window-hack (&rest _)
      "Close popper window via `C-g'."
      ;; `C-g' can deactivate region
      (when (and (called-interactively-p 'interactive)
                 (not (region-active-p))
                 popper-open-popup-alist)
        (let ((window (caar popper-open-popup-alist)))
          (when (window-live-p window)
            (delete-window window)))))
    (advice-add #'keyboard-quit :before #'popper-close-window-hack)))

(use-package pyim
  :init
  (require 'pyim-dict-manager)
  ;; (require 'pyim-basedict)

  (setq default-input-method "pyim"
	    pyim-page-length 7
	    pyim-punctuation-translate-p '(auto yes no) ;中文使用全角标点，英文使用半角标点
	    )
  (if (posframe-workable-p)
      (setq pyim-page-tooltip 'posframe)
    (setq pyim-page-tooltip 'popup))
  ;; (setq pyim-page-tooltip 'popup)

  :config
  (pyim-default-scheme 'quanpin)

  ;; 金手指设置，可以将光标处的编码，比如：拼音字符串，转换为中文。
  ;; (global-set-key (kbd "C-j") 'pyim-convert-string-at-point)
  (global-set-key (kbd "C-\\") 'toggle-input-method)

  ;; 按 "C-<return>" 将光标前的 regexp 转换为可以搜索中文的 regexp.
  (define-key minibuffer-local-map (kbd "C-<return>") 'pyim-cregexp-convert-at-point)

  (define-key pyim-mode-map "." 'pyim-page-next-page)
  (define-key pyim-mode-map "," 'pyim-page-previous-page)
  ;; 用 “;” 来选择第二个候选词
  (define-key pyim-mode-map ";"
    (lambda ()
      (interactive)
      (pyim-select-word-by-number 2)))

  (pyim-extra-dicts-add-dict
   `(:name "tsinghua-dict"
     :file, (expand-file-name "resources/dicts/pyim-tsinghua-dict.pyim" user-emacs-directory)
     :coding utf-8-unix
     :dict-type pinyin-dict
     ))
  (pyim-extra-dicts-add-dict
   `(:name "daily-dict"
     :file, (expand-file-name "resources/dicts/daily.pyim" user-emacs-directory)
     :coding utf-8-unix
     :dict-type pinyin-dict
     ))
  (pyim-extra-dicts-add-dict
   `(:name "math-dict"
     :file, (expand-file-name "resources/dicts/math.pyim" user-emacs-directory)
     :coding utf-8-unix
     :dict-type pinyin-dict
     ))
  (pyim-extra-dicts-add-dict
   `(:name "computer-dict"
     :file, (expand-file-name "resources/dicts/computer.pyim" user-emacs-directory)
     :coding utf-8-unix
     :dict-type pinyin-dict
     ))
  (pyim-extra-dicts-add-dict
   `(:name "computer-nouns-dict"
     :file, (expand-file-name "resources/dicts/computer-nouns.pyim" user-emacs-directory)
     :coding utf-8-unix
     :dict-type pinyin-dict
     ))
  (pyim-extra-dicts-add-dict
   `(:name "coder-dict"
     :file, (expand-file-name "resources/dicts/coder.pyim" user-emacs-directory)
     :coding utf-8-unix
     :dict-type pinyin-dict
     ))
  (pyim-extra-dicts-add-dict
   `(:name "code-lang-dict"
     :file, (expand-file-name "resources/dicts/code-lang.pyim" user-emacs-directory)
     :coding utf-8-unix
     :dict-type pinyin-dict
     ))
  (pyim-extra-dicts-add-dict
   `(:name "data-structure-dict"
     :file, (expand-file-name "resources/dicts/data-structure.pyim" user-emacs-directory)
     :coding utf-8-unix
     :dict-type pinyin-dict
     ))
  (pyim-extra-dicts-add-dict
   `(:name "ai-dict"
     :file, (expand-file-name "resources/dicts/ai.pyim" user-emacs-directory)
     :coding utf-8-unix
     :dict-type pinyin-dict
     ))
  (pyim-extra-dicts-add-dict
   `(:name "nlp-dict"
     :file, (expand-file-name "resources/dicts/nlp.pyim" user-emacs-directory)
     :coding utf-8-unix
     :dict-type pinyin-dict
     ))
  (pyim-extra-dicts-add-dict
   `(:name "chengyu-dict"
     :file, (expand-file-name "resources/dicts/chengyu.pyim" user-emacs-directory)
     :coding utf-8-unix
     :dict-type pinyin-dict
     ))
  (pyim-extra-dicts-add-dict
   `(:name "program-dict"
     :file, (expand-file-name "resources/dicts/program.pyim" user-emacs-directory)
     :coding utf-8-unix
     :dict-type pinyin-dict
     ))
  )

(use-package toggle-one-window
  :quelpa
  (toggle-one-window :fetcher github
  		             :repo "manateelazycat/toggle-one-window"
  		             :files ("*.el"))
  :commands (toggle-one-window))

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

(use-package tree-sitter
  :init
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs)
(use-package tree-sitter-indent
  :hook (rust-mode . tree-sitter-indent-mode))

(use-package ts-fold
  :quelpa (ts-fold :fetcher github
  		           :repo "jcs090218/ts-fold"
  		           :files ("*.el")
                   )
  :pretty-hydra
  ((:foreign-keys warn :quit-key "q")
   ("Toggle"
    (("t" ts-fold-toggle "toggle at point" :exit t))
    "Open"
    (
     ("o" ts-fold-open "open at point" :exit t)
     ("r" ts-fold-open-all "open all" :exit t)
     ("O" ts-fold-open-recursively "recursive open at point" :exit t))
    "Close"
    (("c" ts-fold-close "close at point" :exit t)
     ("m" ts-fold-close-all "close all" :exit t))))
  :init
  (setq ts-fold-indicators-fringe 'right-fringe
        ;; don't obscure lint and breakpoint indicators
        ts-fold-indicators-priority 0
        )
  (dolist (mode-hook tree-sitter--fold-supported-major-mode-hooks)
    (when (boundp mode-hook)
      (add-hook mode-hook #'ts-fold-mode)
      (add-hook mode-hook #'ts-fold-indicators-mode))))

(use-package centered-cursor-mode)
(use-package restart-emacs)
(use-package focus)                     ; Focus on the current region
(use-package carbon-now-sh)
(use-package imenu-list)

(use-package dumb-jump)

(provide 'init-tools)
