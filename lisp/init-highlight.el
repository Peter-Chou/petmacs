;; init-highlight.el --- Setup highlights.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Highlight the current line
(use-package hl-line
  :ensure nil
  :custom-face (hl-line ((t ,@(and emacs/>=27p '(:extend t)))))
  :hook (after-init . global-hl-line-mode))

;; Highlight symbols
(use-package symbol-overlay
  :diminish
  :custom-face
  (symbol-overlay-default-face ((t (:inherit 'region))))
  (symbol-overlay-face-1 ((t (:inherit 'highlight))))
  (symbol-overlay-face-2 ((t (:inherit 'font-lock-builtin-face :inverse-video t))))
  (symbol-overlay-face-3 ((t (:inherit 'warning :inverse-video t))))
  (symbol-overlay-face-4 ((t (:inherit 'font-lock-constant-face :inverse-video t))))
  (symbol-overlay-face-5 ((t (:inherit 'error :inverse-video t))))
  (symbol-overlay-face-6 ((t (:inherit 'dired-mark :inverse-video t :bold nil))))
  (symbol-overlay-face-7 ((t (:inherit 'success :inverse-video t))))
  (symbol-overlay-face-8 ((t (:inherit 'dired-symlink :inverse-video t :bold nil))))
  :bind (("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-N" . symbol-overlay-switch-forward)
         ("M-P" . symbol-overlay-switch-backward)
         ("M-C" . symbol-overlay-remove-all)
         ([M-f3] . symbol-overlay-remove-all))
  :hook ((prog-mode . symbol-overlay-mode)
         ((iedit-mode treemacs-mode) . (lambda ()
                                         "Disabled symbol highlighting."
                                         (symbol-overlay-mode -1)))
         (iedit-mode-end . (lambda ()
                             "Enable symbol highlighting if necessary."
                             (when (derived-mode-p 'prog-mode)
                               (symbol-overlay-mode 1)))))
  :init (setq symbol-overlay-idle-time 0.1)
  :config
  (global-set-key (kbd "M-i") 'symbol-overlay-put)
  (global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
  (global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
  (global-set-key (kbd "<f7>") 'symbol-overlay-mode)
  (global-set-key (kbd "M-c") 'symbol-overlay-remove-all)
  ;; (global-set-key (kbd "<f8>") 'symbol-overlay-remove-all)
  ;; remap help tooltip keybinding from h to H in symbol-overlay-map
  (define-key symbol-overlay-map (kbd "h") 'evil-backward-char)
  (define-key symbol-overlay-map (kbd "H") 'symbol-overlay-map-help)

  ;; Disable symbol highlighting while selecting
  (defadvice set-mark (after disable-symbol-overlay activate)
    (symbol-overlay-mode -1))
  (defadvice deactivate-mark (after enable-symbol-overlay activate)
    (symbol-overlay-mode 1)))

(use-package highlight-indent-guides
  :custom-face
  (highlight-indent-guides-top-character-face ((t (:inherit (font-lock-keyword-face bold)))))
  (highlight-indent-guides-character-face ((t (:inherit (font-lock-comment-face)))))
  :commands highlight-indent-guides--highlighter-default
  :functions (ivy-cleanup-string
	      my-ivy-cleanup-indentation
	      my-indent-guides-for-all-but-first-column)
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character
	highlight-indent-guides-character ?\┆ ;; candidates: , ⋮, ┆, ┊, ┋, ┇
	highlight-indent-guides-responsive 'top
	highlight-indent-guides-auto-enabled nil
	highlight-indent-guides-auto-character-face-perc 10
	highlight-indent-guides-auto-top-character-face-perc 20)
    :config
    ;; Don't display indentations while editing with `company'
    (with-eval-after-load 'company
      (add-hook 'company-completion-started-hook
                (lambda (&rest _)
                  "Trun off indentation highlighting."
                  (when highlight-indent-guides-mode
                    (highlight-indent-guides-mode -1))))
      (add-hook 'company-after-completion-hook
                (lambda (&rest _)
                  "Trun on indentation highlighting."
                  (when (and (derived-mode-p 'prog-mode)
                             (not highlight-indent-guides-mode))
                    (highlight-indent-guides-mode 1)))))

    ;; Don't display first level of indentation
    (defun my-indent-guides-for-all-but-first-column (level responsive display)
      (unless (< level 1)
        (highlight-indent-guides--highlighter-default level responsive display)))
    (setq highlight-indent-guides-highlighter-function
          #'my-indent-guides-for-all-but-first-column)

    ;; Don't display indentations in `swiper'
    ;; https://github.com/DarthFennec/highlight-indent-guides/issues/40
    (with-eval-after-load 'ivy
      (defun my-ivy-cleanup-indentation (str)
        "Clean up indentation highlighting in ivy minibuffer."
        (let ((pos 0)
              (next 0)
              (limit (length str))
              (prop 'highlight-indent-guides-prop))
          (while (and pos next)
            (setq next (text-property-not-all pos limit prop nil str))
            (when next
              (setq pos (text-property-any next limit prop nil str))
              (ignore-errors
                (remove-text-properties next pos '(display nil face nil) str))))))
      (advice-add #'ivy-cleanup-string :after #'my-ivy-cleanup-indentation)))

;; Colorize color names in buffers
(use-package rainbow-mode
  :diminish
  :defines helpful-mode-map
  :functions (my-rainbow-colorize-match my-rainbow-clear-overlays)
  :commands (rainbow-x-color-luminance rainbow-colorize-match rainbow-turn-off)
  :bind (:map help-mode-map
         ("w" . rainbow-mode))
  :hook ((html-mode php-mode) . rainbow-mode)
  :config
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
  (advice-add #'rainbow-turn-off :after #'my-rainbow-clear-overlays))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight matching paren
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t))

;; Pulse current line
(use-package pulse
  :ensure nil
  :preface
  (defun my-pulse-momentary-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point) 'next-error))

  (defun my-pulse-momentary (&rest _)
    "Pulse the current line."
    (if (fboundp 'xref-pulse-momentarily)
        (xref-pulse-momentarily)
      (my-pulse-momentary-line)))

  (defun my-recenter-and-pulse(&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (my-pulse-momentary))

  (defun my-recenter-and-pulse-line (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (my-pulse-momentary-line))
  :hook (((dumb-jump-after-jump
           imenu-after-jump) . my-recenter-and-pulse)
         ((bookmark-after-jump
           magit-diff-visit-file
           next-error) . my-recenter-and-pulse-line))
  :init
  (dolist (cmd '(recenter-top-bottom
                 other-window ace-window windmove-do-window-select
                 pager-page-down pager-page-up
                 symbol-overlay-basic-jump))
    (advice-add cmd :after #'my-pulse-momentary-line))
  (dolist (cmd '(pop-to-mark-command
                 pop-global-mark
                 goto-last-change))
    (advice-add cmd :after #'my-recenter-and-pulse)))

;; Highlight some operations
(use-package volatile-highlights
  :diminish
  :hook (after-init . volatile-highlights-mode))

;; Highlight TODO and similar keywords in comments and strings
(use-package hl-todo
  :custom-face (hl-todo ((t (:box t :inherit 'hl-todo))))
  :bind (:map hl-todo-mode-map
	      ([C-f3] . hl-todo-occur)
	      ("C-c t p" . hl-todo-previous)
	      ("C-c t n" . hl-todo-next)
	      ("C-c t o" . hl-todo-occur))
  :hook (after-init . global-hl-todo-mode)
  :config
  (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
  (dolist (keyword '("WORKAROUND" "HACK" "TRICK"))
    (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces)))

(provide 'init-highlight)

;;; init-highlight.el ends here
