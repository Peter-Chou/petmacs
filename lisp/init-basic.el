;; -*- lexical-binding: t no-byte-compile: t -*-

(require 'subr-x)
(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

(use-package no-littering
  :init
  (setq no-littering-etc-directory (expand-file-name "config/" user-emacs-directory)
        no-littering-var-directory (expand-file-name "data/" user-emacs-directory)
        custom-file (no-littering-expand-etc-file-name "custom.el")
        auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  ;; (require 'no-littering)

  (with-eval-after-load 'recentf-mode
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))

(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;; alternative to undo-tree
(use-package undo-fu)

(use-package solar
  :ensure nil
  :config
  ;; Shanghai, Asia
  (setq calendar-latitude 31.23
        calendar-longitude 121.47))

;; Delete selection if you insert
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; Cross-referencing commands
(use-package xref
  :init
  ;; Use faster search tool
  (setq xref-search-program (cond
                             ((executable-find "ugrep") 'ugrep)
                             ((executable-find "rg") 'ripgrep)
                             (t 'grep)))

  ;; Select from xref candidates in minibuffer
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function #'xref-show-definitions-completing-read))

;; Process
(use-package proced
  :ensure nil
  :init
  (setq-default proced-format 'verbose)
  (setq proced-auto-update-flag t
        proced-auto-update-interval 3))

(with-no-warnings
  ;; Optimization
  (when sys/win32p
    (setq w32-get-true-file-attributes nil   ; decrease file IO workload
          w32-pipe-read-delay 0              ; faster IPC
          w32-pipe-buffer-size (* 64 1024))) ; read more at a time (was 4K)
  (unless sys/macp
    (setq command-line-ns-option-alist nil))
  (unless sys/linuxp
    (setq command-line-x-option-alist nil))

  ;; Increase how much is read from processes in a single chunk (default is 4kb)
  (setq read-process-output-max #x10000)  ; 64kb

  ;; Don't ping things that look like domain names.
  (setq ffap-machine-p-known 'reject)

  ;; Garbage Collector Magic Hack
  (use-package gcmh
    :diminish
    :hook (emacs-startup . gcmh-mode)
    :init
    (setq gcmh-idle-delay 3
          gcmh-high-cons-threshold (* 1 1024 1024 1024) ;; 1gb
          gcmh-verbose nil
          gc-cons-percentage 0.6
          )) ; 16MB
  )

;; Set UTF-8 as the default coding system
;; (set-language-environment "utf-8")
;; (when (fboundp 'set-charset-priority)
;;   (set-charset-priority 'unicode))
;; (setq system-time-locale "C")

;; Set UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq system-time-locale "C")
(unless sys/win32p
  (set-selection-coding-system 'utf-8))

;; Environment
(when (or sys/mac-x-p sys/linux-x-p (daemonp))
  (use-package exec-path-from-shell
    :config
    (dolist (var '("LSP_USE_PLISTS"))
      (add-to-list 'exec-path-from-shell-variables var))
    )

  (use-package cache-path-from-shell
    :quelpa (cache-path-from-shell :fetcher github
  		                           :repo "manateelazycat/cache-path-from-shell"
  		                           :files ("*.el"))
    :commands (exec-path-from-shell-initialize)
    :init
    (exec-path-from-shell-initialize)))

;; Start server
(use-package server
  :ensure nil
  :hook (after-init . server-mode))


(use-package recentf
  :ensure nil
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 100))

(use-package save-place
  :ensure nil
  :hook (after-init . save-place-mode)
  :init (setq save-place-limit 500
              save-place-abbreviate-file-names t))

(use-package simple
  :ensure nil
  :hook ((after-init . size-indication-mode)
         (text-mode . visual-line-mode)
         ((prog-mode markdown-mode conf-mode) . enable-trailing-whitespace))
  :init
  (setq column-number-mode t
        line-number-mode t
        ;; kill-whole-line t               ; Kill line including '\n'
        line-move-visual nil
        track-eol t                     ; Keep cursor at end of lines. Require line-move-visual is nil.
        set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again

  ;; Only list the commands of the current modes
  (when (boundp 'read-extended-command-predicate)
    (setq read-extended-command-predicate
          #'command-completion-default-include-p))

  ;; Visualize TAB, (HARD) SPACE, NEWLINE
  (setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
  (defun enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

  ;; Prettify the process list
  (with-no-warnings
    (add-hook 'process-menu-mode-hook
              (lambda ()
                (setq tabulated-list-format
                      (vconcat `(("" ,(if (icons-displayable-p) 2 0)))
                               tabulated-list-format))))

    (defun my-list-processes--prettify ()
      "Prettify process list."
      (when-let ((entries tabulated-list-entries))
        (setq tabulated-list-entries nil)
        (dolist (p (process-list))
          (when-let* ((val (cadr (assoc p entries)))
                      (icon (if (icons-displayable-p)
                                (concat
                                 " "
                                 (nerd-icons-faicon "nf-fa-bolt" :face 'nerd-icons-lblue))
                              " x"))
                      (name (aref val 0))
                      (pid (aref val 1))
                      (status (aref val 2))
                      (status (list status
                                    'face
                                    (if (memq status '(stop exit closed failed))
                                        'error
                                      'success)))
                      (buf-label (aref val 3))
                      (tty (list (aref val 4) 'face 'font-lock-doc-face))
                      (thread (list (aref val 5) 'face 'font-lock-doc-face))
                      (cmd (list (aref val (if emacs/>=27p 6 5)) 'face 'completions-annotations)))
            (push (list p (if emacs/>=27p
                              (vector icon name pid status buf-label tty thread cmd)
                            (vector icon name pid status buf-label tty cmd)))
		          tabulated-list-entries)))))
    (advice-add #'list-processes--refresh :after #'my-list-processes--prettify)))

(use-package so-long
  :hook (after-init . global-so-long-mode))

(use-package display-fill-column-indicator
  :ensure nil
  :hook
  (prog-mode . display-fill-column-indicator-mode)
  :init
  (setq-default fill-column  80)
  (setq display-fill-column-indicator-character "|"))

;; Misc
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

(setq-default major-mode 'text-mode
              fill-column 80
              tab-width 4
              indent-tabs-mode nil)     ; Permanently indent with spaces, never with TABs

(setq visible-bell t
      inhibit-compacting-font-caches t  ; Don’t compact font caches during GC.
      delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
      make-backup-files nil             ; Forbide to make backup files
      auto-save-default nil             ; Disable auto save
      large-file-warning-threshold nil
      vc-follow-symlinks t
      ad-redefinition-action 'accept

      uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil
      word-wrap-by-category t)

;; Frame
(when (display-graphic-p)
  (add-hook 'window-setup-hook #'fix-fullscreen-cocoa)
  ;; Resize and re-position frames conveniently
  ;; Same keybindings as Rectangle on macOS
  (bind-keys ("C-M-<return>"    . centaur-frame-maximize)
             ("C-M-<backspace>" . centaur-frame-restore)
             ("C-M-<left>"      . centaur-frame-left-half)
             ("C-M-<right>"     . centaur-frame-right-half)
             ("C-M-<up>"        . centaur-frame-top-half)
             ("C-M-<down>"      . centaur-frame-bottom-half)))

;; Sqlite
(when (fboundp 'sqlite-open)
  (use-package emacsql-sqlite-builtin))

(setq confirm-kill-processes nil)

(provide 'init-basic)
