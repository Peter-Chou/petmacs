;; -*- lexical-binding: t no-byte-compile: t -*-

(eval-when-compile
  (require 'init-funcs))

;; Highlight the current line
(use-package hl-line
  :ensure nil
  :hook ((after-init . global-hl-line-mode)
         ((dashboard-mode eshell-mode shell-mode term-mode vterm-mode) .
          (lambda () (setq-local global-hl-line-mode nil)))))

;; Highlight matching parens
(use-package paren
  :ensure nil
  :functions childframe-workable-p
  :custom-face
  (show-paren-match ((((class color) (background light))
                      (:box (:line-width (-1 . -1) :color "gray73")))
                     (((class color) (background dark))
                      (:box (:line-width (-1 . -1) :color "gray56")))))
  :hook (after-init . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
              show-paren-when-point-in-periphery t)
  :config
  (if emacs/>=29p
      (setq blink-matching-paren-highlight-offscreen t
            show-paren-context-when-offscreen
            (if (childframe-workable-p) 'child-frame 'overlay))
    (with-no-warnings
      ;; Display matching line for off-screen paren.
      (defun display-line-overlay (pos str &optional face)
        "Display line at POS as STR with FACE.

FACE defaults to inheriting from default and highlight."
        (let ((ol (save-excursion
                    (goto-char pos)
                    (make-overlay (line-beginning-position)
                                  (line-end-position)))))
          (overlay-put ol 'display str)
          (overlay-put ol 'face
                       (or face '(:inherit highlight)))
          ol))

      (defvar-local show-paren--off-screen-overlay nil)
      (defun show-paren-off-screen (&rest _args)
        "Display matching line for off-screen paren."
        (when (overlayp show-paren--off-screen-overlay)
          (delete-overlay show-paren--off-screen-overlay))
        ;; Check if it's appropriate to show match info,
        (when (and (overlay-buffer show-paren--overlay)
                   (not (or cursor-in-echo-area
                            executing-kbd-macro
                            noninteractive
                            (minibufferp)
                            this-command))
                   (and (not (bobp))
                        (memq (char-syntax (char-before)) '(?\) ?\$)))
                   (= 1 (logand 1 (- (point)
                                     (save-excursion
                                       (forward-char -1)
                                       (skip-syntax-backward "/\\")
                                       (point))))))
          ;; Rebind `minibuffer-message' called by `blink-matching-open'
          ;; to handle the overlay display.
          (cl-letf (((symbol-function #'minibuffer-message)
                     (lambda (msg &rest args)
                       (let ((msg (apply #'format-message msg args)))
                         (setq show-paren--off-screen-overlay
                               (display-line-overlay
                                (window-start) msg ))))))
            (blink-matching-open))))
      (advice-add #'show-paren-function :after #'show-paren-off-screen))))

;; Highlight symbols
(use-package symbol-overlay
  :diminish
  :custom-face
  (symbol-overlay-default-face ((t (:inherit region :background unspecified :foreground unspecified))))
  (symbol-overlay-face-1 ((t (:inherit nerd-icons-blue :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-2 ((t (:inherit nerd-icons-pink :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-3 ((t (:inherit nerd-icons-yellow :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-4 ((t (:inherit nerd-icons-purple :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-5 ((t (:inherit nerd-icons-red :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-6 ((t (:inherit nerd-icons-orange :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-7 ((t (:inherit nerd-icons-green :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-8 ((t (:inherit nerd-icons-cyan :background unspecified :foreground unspecified :inverse-video t))))
  :bind (("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-N" . symbol-overlay-switch-forward)
         ("M-P" . symbol-overlay-switch-backward)
         ("M-C" . symbol-overlay-remove-all))
  :hook (((prog-mode yaml-mode yaml-ts-mode) . symbol-overlay-mode)
         (iedit-mode            . turn-off-symbol-overlay)
         (iedit-mode-end        . turn-on-symbol-overlay))
  :init (setq symbol-overlay-idle-time 0.3)
  :config
  (with-no-warnings
    ;; Disable symbol highlighting while selecting
    (defun turn-off-symbol-overlay (&rest _)
      "Turn off symbol highlighting."
      (interactive)
      (symbol-overlay-mode -1))

    (defun turn-on-symbol-overlay (&rest _)
      "Turn on symbol highlighting."
      (interactive)
      (when (derived-mode-p 'prog-mode 'yaml-mode 'yaml-ts-mode)
        (symbol-overlay-mode 1)))

    (advice-add #'activate-mark :after #'turn-off-symbol-overlay)
    (advice-add #'deactivate-mark :after #'turn-on-symbol-overlay)))

;; Mark occurrences of current region (selection)
(use-package
  region-occurrences-highlighter
  :diminish
  :bind (:map region-occurrences-highlighter-nav-mode-map
         ("M-n" . region-occurrences-highlighter-next)
         ("M-p" . region-occurrences-highlighter-prev))
  :hook (after-init . global-region-occurrences-highlighter-mode))

(use-package indent-bars
  :pin gnu
  :hook (((
           go-mode go-ts-mode
           json-mode json-ts-mode
           python-mode python-ts-mode
           toml-mode toml-ts-mode
           yaml-mode yaml-ts-mode) . (lambda () (unless (too-long-file-p)
                                             (indent-bars-mode 1))))
         ((java-mode java-ts-mode) . (lambda ()
                                       (indent-bars-mode -1))))
  :init
  (setq
   ;; indent-bars-no-descend-string t
   ;; indent-bars-width-frac 0.25
   ;; indent-bars-color
   ;; '(highlight :face-bg t :blend 0.225)

   ;; indent-bars-pattern ". . . . "
   indent-bars-pattern "."
   indent-bars-color '(highlight :face-bg t :blend 0.25)
   indent-bars-width-frac 0.25
   indent-bars-pad-frac 0.2
   indent-bars-zigzag 0.1
   indent-bars-color-by-depth '(:palette ("red" "green" "orange" "cyan" "hot pink" "peru" "Light Green") :blend 0.8)
   indent-bars-display-on-blank-lines nil)

  (if (and emacs/>=30p (display-graphic-p))
      (setq indent-bars-highlight-current-depth '(:face petmacs-favor-color-face :pattern ". . . . " :pad 0.1 :width 0.45))
    (setq indent-bars-prefer-character t
          indent-bars-no-stipple-char ?\┋
          indent-bars-highlight-current-depth '(:face petmacs-favor-color-face :pattern "." :pad 0.1)))

  (when (petmacs-treesit-available-p)
    (setq indent-bars-treesit-support t
          indent-bars-ts-color '(inherit fringe :face-bg t :blend 0.2)
          indent-bars-treesit-ignore-blank-lines-types '("module")
          indent-bars-treesit-scope '((python function_definition class_definition for_statement
				                              if_statement with_statement while_statement))

          indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
				                             list list_comprehension
				                             dictionary dictionary_comprehension
				                             parenthesized_expression subscript)
                                     (c argument_list parameter_list init_declarator parenthesized_expression)
                                     (toml table array comment)
                                     (yaml block_mapping_pair comment)
                                     )))
  :config
  (require 'indent-bars-ts))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Colorize color names in buffers
(if emacs/>=28p
    (use-package colorful-mode
      :diminish
      :hook (after-init . global-colorful-mode)
      :init
      (setq colorful-use-prefix t
            ;; colorful-prefix-string (format "%s" (nerd-icons-faicon "nf-fa-circle"))
            )
      :config (dolist (mode '(html-mode php-mode help-mode helpful-mode))
                (add-to-list 'global-colorful-modes mode)))

  (use-package rainbow-mode
    :diminish
    :defines helpful-mode-map
    :bind (:map help-mode-map
           ("w" . rainbow-mode))
    :hook ((mhtml-mode html-mode html-ts-mode php-mode latex-mode help-mode helpful-mode) . rainbow-mode)
    :init (with-eval-after-load 'helpful
            (bind-key "w" #'rainbow-mode helpful-mode-map))
    :config
    (with-no-warnings
      ;; HACK: Use overlay instead of text properties to override `hl-line' faces.
      ;; @see https://emacs.stackexchange.com/questions/36420
      (defun my-rainbow-colorize-match (color &optional match)
        (let* ((match (or match 0))
               (ov (make-overlay (match-beginning match) (match-end match))))
          (overlay-put ov 'ovrainbow t)
          (overlay-put ov 'face `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
                                                    "white" "black"))
                                  (:background ,color)))))
      (advice-add #'rainbow-colorize-match :override #'my-rainbow-colorize-match)

      (defun my-rainbow-clear-overlays ()
        "Clear all rainbow overlays."
        (remove-overlays (point-min) (point-max) 'ovrainbow t))
      (advice-add #'rainbow-turn-off :after #'my-rainbow-clear-overlays))))

;; Highlight TODO and similar keywords in comments and strings
(use-package hl-todo
  :autoload hl-todo-flymake hl-todo-search-and-highlight
  :functions rg-read-files rg-project
  :bind (:map hl-todo-mode-map
         ([C-f3]    . hl-todo-occur)
         ("C-c t p" . hl-todo-previous)
         ("C-c t n" . hl-todo-next)
         ("C-c t o" . hl-todo-occur)
         ("C-c t i" . hl-todo-insert))
  :hook (after-init . global-hl-todo-mode)
  :init (setq hl-todo-color-background t
              hl-todo-include-modes '(prog-mode conf-mode)
              hl-todo-exclude-modes '(org-mode
                                      markdown-mode
                                      yaml-mode
                                      yaml-ts-mode)
              hl-todo-keyword-faces '(("TODO" . ((t (:foreground "#ffffff" :background "#e45649" :weight bold))))
                                      ("FIXME" . ((t (:foreground "#ffffff" :background "#e45649" :weight bold))))
                                      ("ISSUE" . ((t (:foreground "#ffffff" :background "#e45649" :weight bold))))
                                      ("DEFECT" . ((t (:foreground "#ffffff" :background "#e45649" :weight bold))))
                                      ("TRICK" . ((t (:foreground "#ffffff" :background "#d0bf8f" :weight bold))))
                                      ("WORKAROUND" . ((t (:foreground "#ffffff" :background "#d0bf8f" :weight bold))))
                                      ("DEBUG" . ((t (:foreground "#ffffff" :background "#7cb8bb" :weight bold))))
                                      ("STUB" . ((t (:foreground "#ffffff" :background "#7cb8bb" :weight bold)))))
              ;; hl-todo-require-punctuation t
              hl-todo-highlight-punctuation ":")
  :config
  (with-eval-after-load 'magit
    (add-hook 'magit-log-wash-summary-hook
              #'hl-todo-search-and-highlight t)
    (add-hook 'magit-revision-wash-message-hook
              #'hl-todo-search-and-highlight t))

  (defun hl-todo-rg (regexp &optional files dir)
    "Use `rg' to find all TODO or similar keywords."
    (interactive
     (progn
       (unless (require 'rg nil t)
         (error "`rg' is not installed"))
       (let ((regexp (replace-regexp-in-string "\\\\[<>]*" "" (hl-todo--regexp))))
         (list regexp
               (rg-read-files)
               (read-directory-name "Base directory: " nil default-directory t)))))
    (rg regexp files dir))

  (defun hl-todo-rg-project ()
    "Use `rg' to find all TODO or similar keywords in current project."
    (interactive)
    (unless (require 'rg nil t)
      (error "`rg' is not installed"))
    (rg-project (replace-regexp-in-string "\\\\[<>]*" "" (hl-todo--regexp)) "everything")))

;; Pulse current line
(use-package pulse
  :ensure nil
  :custom-face
  (pulse-highlight-start-face ((t (:inherit region))))
  (pulse-highlight-face ((t (:inherit region :extend t))))
  :hook (((dumb-jump-after-jump imenu-after-jump) . my-recenter-and-pulse)
         ((bookmark-after-jump magit-diff-visit-file next-error) . my-recenter-and-pulse-line))
  :init
  (with-no-warnings
    (defun my-pulse-momentary-line (&rest _)
      "Pulse the current line."
      (pulse-momentary-highlight-one-line (point)))

    (defun my-pulse-momentary (&rest _)
      "Pulse the region or the current line."
      (if (fboundp 'xref-pulse-momentarily)
          (xref-pulse-momentarily)
        (my-pulse-momentary-line)))

    (defun my-recenter-and-pulse(&rest _)
      "Recenter and pulse the region or the current line."
      (recenter)
      (my-pulse-momentary))

    (defun my-recenter-and-pulse-line (&rest _)
      "Recenter and pulse the current line."
      (recenter)
      (my-pulse-momentary-line))

    (dolist (cmd '(recenter-top-bottom
                   other-window switch-to-buffer
                   aw-select toggle-window-split
                   windmove-do-window-select
                   pager-page-down pager-page-up
                   treemacs-select-window))
      (advice-add cmd :after #'my-pulse-momentary-line))

    (dolist (cmd '(pop-to-mark-command
                   pop-global-mark
                   goto-last-change))
      (advice-add cmd :after #'my-recenter-and-pulse))))

;; Pulse modified region
(use-package goggles
  :diminish
  :hook ((prog-mode text-mode conf-mode) . goggles-mode))

(provide 'init-highlight)
