;; init-default.el --- Setup defaults.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(add-to-list 'default-frame-alist '(height . 55))
(add-to-list 'default-frame-alist '(width . 100))

;; use exec-path-from-shell in linux / mac
(when (or (eq system-type 'gnu/linux) (eq system-type 'darwin) (daemonp))
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH"
					   "WORKON_HOME" "JAVA_HOME"
					   "LLVM_HOME" "LD_LIBRARY_PATH")
	  exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

;; maximize emacs after initialization
;; (toggle-frame-maximized)

;; fix print gibberish when in windows system
;; https://blog.csdn.net/sanwu2010/article/details/23994977
;; (if sys/win32p
;;     (progn
;;       (set-language-environment 'Chinese-GB)
;;       ;; default-buffer-file-coding-system变量在emacs23.2之后已被废弃，使用buffer-file-coding-system代替
;;       (set-default buffer-file-coding-system 'utf-8-unix)
;;       (set-default-coding-systems 'utf-8-unix)
;;       (setq-default pathname-coding-system 'euc-cn)
;;       (setq file-name-coding-system 'euc-cn)
;;       ;; 另外建议按下面的先后顺序来设置中文编码识别方式。
;;       ;; 重要提示:写在最后一行的，实际上最优先使用; 最前面一行，反而放到最后才识别。
;;       ;; utf-16le-with-signature 相当于 Windows 下的 Unicode 编码，这里也可写成
;;       ;; utf-16 (utf-16 实际上还细分为 utf-16le, utf-16be, utf-16le-with-signature等多种)
;;       (prefer-coding-system 'cp950)
;;       (prefer-coding-system 'gb2312)
;;       (prefer-coding-system 'cp936)
;;       (prefer-coding-system 'gb18030)
;; 					;(prefer-coding-system 'utf-16le-with-signature)
;;       (prefer-coding-system 'utf-16)
;;       ;; 新建文件使用utf-8-unix方式
;;       ;; 如果不写下面两句，只写
;;       ;; (prefer-coding-system 'utf-8)
;;       ;; 这一句的话，新建文件以utf-8编码，行末结束符平台相关
;;       (prefer-coding-system 'utf-8-dos)
;;       (prefer-coding-system 'utf-8-unix)
;;       )
;;   (progn
;;     (set-language-environment petmacs-default-language-env)
;;     (set-default-coding-systems petmacs-default-coding-env)))

;; (modify-coding-system-alist 'process "python" '(utf-8 . chinese-gbk-dos))

(if sys/win32p
    (progn
      ;; for python output chinese
      ;; Emacs buffer -> python : encoding = utf8
      ;; python output -> Eamcs : decoding = chinese-gbk-dos
      (modify-coding-system-alist 'process "python" '(chinese-gbk-dos . utf-8))
      ;; format buffer
      ;; Emacs buffer -> python : encoding = utf8
      ;; python output -> Eamcs : decoding = chinese-gbk-dos
      (modify-coding-system-alist 'process "yapf" '(utf-8 . utf-8)))
  (progn
    ;; UTF-8 as the default coding system
    (when (fboundp 'set-charset-priority)
      (set-charset-priority 'unicode))
    (set-language-environment petmacs-default-language-env)
    (set-default-coding-systems petmacs-default-coding-env))
  (set-keyboard-coding-system petmacs-default-coding-env)
  (set-clipboard-coding-system petmacs-default-coding-env)
  (set-terminal-coding-system petmacs-default-coding-env)
  (set-buffer-file-coding-system petmacs-default-coding-env)
  (set-selection-coding-system petmacs-default-coding-env)
  (modify-coding-system-alist 'process "*" petmacs-default-coding-env)
  (set-file-name-coding-system petmacs-default-coding-env)

  (setq locale-coding-system 'utf-8
	default-process-coding-system '(utf-8 . utf-8)))

(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder
(setq make-backup-files nil)               ; Forbide to make backup files
(setq auto-save-default nil)               ; Disable auto save
(setq create-lockfiles nil)                ; Disable lock files .#filename

(setq-default major-mode 'text-mode)

;; UI
(unless (eq window-system 'ns)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(setq-default fill-column 80)

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

  ;; Visualize TAB, (HARD) SPACE, NEWLINE
  (setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
  (defun enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))

;; Don't open a file in a new frame
(when (boundp 'ns-pop-up-frames)
  (setq ns-pop-up-frames nil))

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

;; Basic modes

;; Start server
(use-package server
  :ensure nil
  :hook (after-init . server-mode))

;; History
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; revert buffer without confimation
(setq revert-without-query '(".*"))

;; Pass a URL to a WWW browser
(use-package browse-url
  :ensure nil
  :defines dired-mode-map
  :bind (("C-c C-z ." . browse-url-at-point)
         ("C-c C-z b" . browse-url-of-buffer)
         ("C-c C-z r" . browse-url-of-region)
         ("C-c C-z u" . browse-url)
         ("C-c C-z v" . browse-url-of-file))
  :init
  (with-eval-after-load 'dired
    (bind-key "C-c C-z f" #'browse-url-of-file dired-mode-map)))

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
  (add-to-list 'recentf-filename-handlers 'abbreviate-file-name))

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
              savehist-autosave-interval 300))

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :diminish
  :hook ((after-init . global-aggressive-indent-mode)
         ;; FIXME: Disable in big files due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         (find-file . (lambda ()
                        (if (> (buffer-size) (* 3000 80))
                            (aggressive-indent-mode -1)))))
  :config
  ;; Disable in some modes
  (dolist (mode '(asm-mode web-mode html-mode css-mode go-mode scala-mode prolog-inferior-mode))
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

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(use-package pretty-hydra
  :init
  (with-no-warnings
    (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                        &key face height v-adjust)
      "Add an icon in the hydra title."
      (let ((face (or face `(:foreground ,(face-background 'highlight))))
            (height (or height 1.0))
            (v-adjust (or v-adjust 0.0)))
        (concat
         (when (and (display-graphic-p) icon-type icon-name)
           (let ((f (intern (format "all-the-icons-%s" icon-type))))
             (when (fboundp f)
               (concat
                (apply f (list icon-name :face face :height height :v-adjust v-adjust))
                " "))))
         (propertize title 'face face))))))

;; Treat undo history as a tree
(use-package undo-tree
  :diminish
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-enable-undo-in-region nil
        undo-tree-auto-save-history nil)

  ;; WORKAROUND:  keep the diff window
  (with-no-warnings
    (make-variable-buffer-local 'undo-tree-visualizer-diff)
    (setq-default undo-tree-visualizer-diff t)))

;; Goto last change
(use-package goto-last-change)

;; Handling capitalized subwords in a nomenclature
(use-package subword
  :ensure nil
  :diminish
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

;; (defun icons-displayable-p ()
;;   "Return non-nil if `all-the-icons' is displayable."
;;   (and (display-graphic-p)
;;        (require 'all-the-icons nil t)))


(use-package frame-cmds
  :ensure nil
  :init (require 'frame-cmds))

(when emacs/>=27p
  (use-package so-long
    :ensure nil
    :hook (after-init . global-so-long-mode)
    :config (setq so-long-threshold 400)))

;; Process
(use-package proced
  :ensure nil
  :init
  (setq-default proced-format 'verbose)
  (setq proced-auto-update-flag t
        proced-auto-update-interval 3))

(use-package winner-mode
  :ensure nil
  :hook (after-init . winner-mode))

(provide 'init-default)
;;; init-default.el ends here
