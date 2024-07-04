;; -*- lexical-binding: t no-byte-compile: t -*-

(require 'init-funcs)

;; Highlight the current line
(use-package hl-line
  :ensure nil
  :hook ((after-init . global-hl-line-mode)
         ((dashboard-mode eshell-mode shell-mode term-mode vterm-mode) .
          (lambda () (setq-local global-hl-line-mode nil)))))

;; Highlight matching parens
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
              show-paren-when-point-in-periphery t)
  :config
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
    (advice-add #'show-paren-function :after #'show-paren-off-screen)))


(use-package symbol-overlay
  :custom-face
  (symbol-overlay-face-1 ((t (:inherit nerd-icons-blue :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-2 ((t (:inherit nerd-icons-pink :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-3 ((t (:inherit nerd-icons-yellow :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-4 ((t (:inherit nerd-icons-purple :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-5 ((t (:inherit nerd-icons-red :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-6 ((t (:inherit nerd-icons-orange :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-7 ((t (:inherit nerd-icons-green :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-8 ((t (:inherit nerd-icons-cyan :background unspecified :foreground unspecified :inverse-video t))))
  :pretty-hydra
  ((:title (pretty-hydra-title "symbol overlay")
    :foreign-keys warn :quit-key ("q" "C-g"))
   ("Actions"
    (("c" symbol-overlay-save-symbol)
     ("e" symbol-overlay-echo-mark)
     ("o" symbol-overlay-put)
     ("O" symbol-overlay-remove-all)
     ("r" symbol-overlay-query-replace)
     ("R" symbol-overlay-rename)
     ("s" symbol-overlay-isearch-literally)
     ("t" symbol-overlay-toggle-in-scope)
     ("z" recenter-top-bottom))
    "Jump"
    (("n" symbol-overlay-jump-next)
     ("N" symbol-overlay-jump-prev)
     ("p" symbol-overlay-jump-prev)
     ("d" symbol-overlay-jump-to-definition)
     ("b" symbol-overlay-switch-backward)
     ("f" symbol-overlay-switch-forward))))
  :bind (("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-N" . symbol-overlay-switch-forward)
         ("M-P" . symbol-overlay-switch-backward)
         ("M-C" . symbol-overlay-remove-all))
  :hook ((prog-mode yaml-mode) . symbol-overlay-mode)
  (iedit-mode . turn-off-symbol-overlay)
  (iedit-mode-end . turn-on-symbol-overlay)
  :init (setq symbol-overlay-idle-time 0.1)
  :config
  (with-no-warnings
    ;; Disable symbol highlighting while selecting
    (defun turn-off-symbol-overlay (&rest _)
      "Turn off symbol highlighting."
      (interactive)
      (symbol-overlay-mode -1))
    (advice-add #'set-mark :after #'turn-off-symbol-overlay)

    (defun turn-on-symbol-overlay (&rest _)
      "Turn on symbol highlighting."
      (interactive)
      (when (derived-mode-p 'prog-mode 'yaml-mode)
        (symbol-overlay-mode 1)))
    (advice-add #'deactivate-mark :after #'turn-on-symbol-overlay)))

(if petmacs-quelpa-use-gitee-mirror
    (use-package indent-bars
      :quelpa (indent-bars :fetcher git :url "https://gitee.com/Peter-Chou/indent-bars.git" :files ("*.el")))
  (use-package indent-bars
    :quelpa (indent-bars :fetcher github :repo "jdtsmith/indent-bars" :files ("*.el"))))

(use-package indent-bars
  :ensure nil
  :hook (((
           go-mode go-ts-mode
           json-mode json-ts-mode
           python-mode python-ts-mode
           yaml-mode yaml-ts-mode) . (lambda () (unless (too-long-file-p)
                                                  (indent-bars-mode 1))))
         ((java-mode java-ts-mode) . (lambda ()
                                       (indent-bars-mode -1))))
  :init
  (setq indent-bars-display-on-blank-lines nil
        indent-bars-no-stipple-char ?\┋
        indent-bars-width-frac 0.25
        indent-bars-color
        '(highlight :face-bg t :blend 0.85)
        indent-bars-highlight-current-depth '(:face petmacs-favor-color-face :pattern "."))
  (when (petmacs-treesit-available-p)
    (setq indent-bars-treesit-support t
          indent-bars-no-descend-string nil
          indent-bars-treesit-ignore-blank-lines-types '("module")
          indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
				                             list list_comprehension
				                             dictionary dictionary_comprehension
				                             parenthesized_expression subscript)))))

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
            colorful-prefix-string (format "%s" (nerd-icons-faicon "nf-fa-circle")))
      (dolist (mode '(html-mode php-mode help-mode helpful-mode))
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
  :bind (:map hl-todo-mode-map
         ([C-f3]    . hl-todo-occur)
         ("C-c t p" . hl-todo-previous)
         ("C-c t n" . hl-todo-next)
         ("C-c t o" . hl-todo-occur)
         ("C-c t i" . hl-todo-insert))
  :hook ((after-init . global-hl-todo-mode)
         (hl-todo-mode . (lambda ()
                           (add-hook 'flymake-diagnostic-functions
                                     #'hl-todo-flymake nil t))))
  :init (setq hl-todo-color-background t
              hl-todo-include-modes '(prog-mode conf-mode)
              hl-todo-exclude-modes '(org-mode
                                      markdown-mode
                                      yaml-mode
                                      yaml-ts-mode)
              ;; hl-todo-require-punctuation t
              hl-todo-highlight-punctuation ":")
  :config
  (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
    (add-to-list 'hl-todo-keyword-faces `(,keyword . "#e45649")))
  (dolist (keyword '("TRICK" "WORKAROUND"))
    (add-to-list 'hl-todo-keyword-faces `(,keyword . "#d0bf8f")))
  (dolist (keyword '("DEBUG" "STUB"))
    (add-to-list 'hl-todo-keyword-faces `(,keyword . "#7cb8bb")))

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

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :custom-face
  (diff-hl-change ((t (:inherit custom-changed :foreground unspecified :background unspecified))))
  (diff-hl-insert ((t (:inherit diff-added :background unspecified))))
  (diff-hl-delete ((t (:inherit diff-removed :background unspecified))))
  :hook ((after-init . global-diff-hl-mode)
         (after-init . global-diff-hl-show-hunk-mouse-mode)
         (dired-mode . diff-hl-dired-mode))
  :init (setq diff-hl-draw-borders nil
              ;; diff-hl-side 'right
              ;; diff-hl-margin-symbols-alist
              ;; '((insert . "+") (delete . "-") (change . "=")
              ;;   (unknown . "?") (ignored . "i"))
              diff-hl-margin-symbols-alist
              '((insert . " ") (delete . " ") (change . " ")
                (unknown . " ") (ignored . " ")))
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)
  ;; (diff-hl-margin-mode 1)

  ;; Set fringe style
  ;; (setq-default fringes-outside-margins t)

  (with-no-warnings
    ;; (defun my-diff-hl-fringe-bmp-function (_type _pos)
    ;;   "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
    ;;   (define-fringe-bitmap 'my-diff-hl-bmp
    ;;     (vector (if sys/macp #b11100000 #b11111100))
    ;;     1 8
    ;;     '(center t)))
    ;; (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function)

    ;; (unless (display-graphic-p)
    ;;   ;; Fall back to the display margin since the fringe is unavailable in tty
    ;;   (diff-hl-margin-mode 1)
    ;;   ;; Avoid restoring `diff-hl-margin-mode'
    ;;   (with-eval-after-load 'desktop
    ;;     (add-to-list 'desktop-minor-mode-table
    ;;                  '(diff-hl-margin-mode nil))))

    ;; Integration with magit
    (with-eval-after-load 'magit
      (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
      (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))))

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
(if emacs/>=27p
    (use-package goggles
      :diminish
      :hook ((prog-mode text-mode) . goggles-mode))
  (use-package volatile-highlights
    :diminish
    :hook (after-init . volatile-highlights-mode)))

(provide 'init-highlight)
