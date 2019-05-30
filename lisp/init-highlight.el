;; init-highlight.el --- Setup highlights.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Highlight the current line
(use-package hl-line
  :ensure nil
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
  :functions (symbol-overlay-switch-first symbol-overlay-switch-last)
  :commands (symbol-overlay-get-symbol
	     symbol-overlay-assoc
	     symbol-overlay-get-list
	     symbol-overlay-jump-call)
  :hook ((prog-mode . symbol-overlay-mode))
  :init
  (setq symbol-overlay-idle-time 0.01)
  :config
  (global-set-key (kbd "M-i") 'symbol-overlay-put)
  (global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
  (global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
  (global-set-key (kbd "<f7>") 'symbol-overlay-mode)
  (global-set-key (kbd "M-c") 'symbol-overlay-remove-all)
  ;; (global-set-key (kbd "<f8>") 'symbol-overlay-remove-all)

  ;; remap help tooltip keybinding from h to H in symbol-overlay-map
  (define-key symbol-overlay-map (kbd "h") 'evil-backward-char)
  (define-key symbol-overlay-map (kbd "H") 'symbol-overlay-map-help))

(use-package highlight-indent-guides
  :custom-face
  (highlight-indent-guides-top-character-face ((t (:inherit (font-lock-keyword-face bold)))))
  (highlight-indent-guides-character-face ((t (:inherit (font-lock-comment-face)))))
  :defer t
  :hook (((python-mode yaml-mode) . highlight-indent-guides-mode)
	 (ein:notebook-multilang-mode . (lambda () (highlight-indent-guides-mode -1))))
  :config
  (progn
    (setq highlight-indent-guides-method 'character

	  highlight-indent-guides-character ?\┆ ;; candidates: , ⋮, ┆, ┊, ┋, ┇
	  highlight-indent-guides-responsive 'top
	  highlight-indent-guides-auto-enabled nil
	  highlight-indent-guides-auto-character-face-perc 10
	  highlight-indent-guides-auto-top-character-face-perc 20))
  ;; Don't display first level of indentation
  (defun petmacs//indent-guides-for-all-but-first-column (level responsive display)
    (unless (< level 1)
      (highlight-indent-guides--highlighter-default level responsive display)))
  (setq highlight-indent-guides-highlighter-function #'petmacs//indent-guides-for-all-but-first-column))

;; Colorize color names in buffers
(use-package rainbow-mode
  :diminish
  :hook ((prog-mode help-mode) . rainbow-mode)
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
    (remove-overlays (point-min) (point-max) 'ovrainbow t))
  (advice-add #'rainbow-turn-off :after #'my-rainbow-clear-overlays))

;; Highlight some operations
(use-package volatile-highlights
  :diminish
  :hook (after-init . volatile-highlights-mode))

;; Highlight matching paren
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t))

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
