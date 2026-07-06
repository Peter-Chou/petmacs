;; init-highlight.el --- Initialize highlighting configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Highlighting configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-funcs))

;; Highlight the current line
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

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
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :config
  (if (boundp 'show-paren-context-when-offscreen)
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
  :functions (easy-kill easy-kill-destroy-candidate)
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
  (advice-add #'deactivate-mark :after #'turn-on-symbol-overlay)
  (advice-add #'easy-kill :after #'turn-off-symbol-overlay)
  (advice-add #'easy-kill-destroy-candidate :after #'turn-on-symbol-overlay))

;; Mark occurrences of current region (selection)
(use-package
  region-occurrences-highlighter
  :diminish
  :bind (:map region-occurrences-highlighter-nav-mode-map
         ("M-n" . region-occurrences-highlighter-next)
         ("M-p" . region-occurrences-highlighter-prev))
  :hook (after-init . global-region-occurrences-highlighter-mode))

;; Display fill-column indicator
(use-package display-fill-column-indicator
  :ensure nil
  :functions adjust-fill-column-indicator-stipple
  ;; :hook (prog-mode . display-fill-column-indicator-mode)
  :config
  ;; Setup fill column indicator with stipple
  (when (or (and sys/mac-x-p emacs/>=31p)
            (and sys/linux-x-p sys/win32p emacs/>=30p))
    (setq-default display-fill-column-indicator-character ?\s)
    (defun adjust-fill-column-indicator-stipple ()
      "Adjust the fill-column-indicator face with stipple using `set-face-attribute'."
      (let* ((w (window-font-width))
             (stipple `(,w 1 ,(apply #'unibyte-string
                                     (append (make-list (1- (/ (+ w 7) 8)) ?\0)
                                             '(1))))))
        (set-face-attribute 'fill-column-indicator nil :stipple stipple)))
    (add-hook 'emacs-startup-hook #'adjust-fill-column-indicator-stipple)
    (add-hook 'text-scale-mode-hook #'adjust-fill-column-indicator-stipple)))


(use-package indent-bars
  ;; :pin gnu
  :hook (((
           go-mode go-ts-mode
           json-mode json-ts-mode
           python-base-mode
           toml-mode toml-ts-mode
           yaml-mode yaml-ts-mode) . indent-bars-mode)
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
                                     ))))

(use-package colorful-mode
  :diminish
  :hook (after-init . global-colorful-mode)
  :init (setq colorful-use-prefix t))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight TODO and similar keywords in comments and strings
(use-package hl-todo
  :autoload hl-todo-flymake hl-todo-search-and-highlight
  :functions rg rg-read-files rg-project
  :custom-face
  (hl-todo ((t (:inherit default :height 0.9 :width condensed :weight bold :underline nil :inverse-video t))))
  :hook (after-init . global-hl-todo-mode)
  :init (setq hl-todo-color-background t
              hl-todo-include-modes '(prog-mode conf-mode)
              hl-todo-exclude-modes '(org-mode
                                      markdown-mode
                                      yaml-mode
                                      yaml-ts-mode)
              hl-todo-keyword-faces '(("TODO" . ((t (:foreground "#ffffff" :background "#e45649" :weight bold))))
                                      ("FIXME" . ((t (:foreground "#ffffff" :background "#e45649" :weight bold))))
                                      ("BUG" . ((t (:foreground "#ffffff" :background "#e45649" :weight bold))))
                                      ("ISSUE" . ((t (:foreground "#ffffff" :background "#e45649" :weight bold))))
                                      ("DEFECT" . ((t (:foreground "#ffffff" :background "#e45649" :weight bold))))
                                      ("TRICK" . ((t (:foreground "#ffffff" :background "#d0bf8f" :weight bold))))
                                      ("WORKAROUND" . ((t (:foreground "#ffffff" :background "#d0bf8f" :weight bold))))
                                      ("DEBUG" . ((t (:foreground "#ffffff" :background "#7cb8bb" :weight bold))))
                                      ("STUB" . ((t (:foreground "#ffffff" :background "#7cb8bb" :weight bold)))))
              hl-todo-require-punctuation t
              hl-todo-highlight-punctuation ":")
  :config
  ;; Integrate into flymake
  (with-eval-after-load 'flymake
    (add-hook 'flymake-diagnostic-functions #'hl-todo-flymake))

  ;; Integrate into magit
  (with-eval-after-load 'magit
    (add-hook 'magit-log-wash-summary-hook #'hl-todo-search-and-highlight t)
    (add-hook 'magit-revision-wash-message-hook #'hl-todo-search-and-highlight t))

  (defun hl-todo-rg (regexp &optional files dir)
    "Use `rg' to find all TODO or similar keywords."
    (interactive
     (progn
       (unless (require 'rg nil t)
         (error "`rg' is not installed"))
       (let ((regexp (replace-regexp-in-string "\\\\[_<>]*" "" (hl-todo--regexp))))
         (list regexp
               (rg-read-files)
               (read-directory-name "Base directory: " nil default-directory t)))))
    (rg regexp files dir))

  (defun hl-todo-rg-project ()
    "Use `rg' to find all TODO or similar keywords in current project."
    (interactive)
    (unless (require 'rg nil t)
      (error "`rg' is not installed"))
    (rg-project (replace-regexp-in-string "\\\\[_<>]*" "" (hl-todo--regexp)) "everything")))

;; Pulse highlight on selection
(use-package pulsar
  :custom-face
  (pulsar-generic ((t :inherit region :extend t)))
  :custom (pulsar-delay pulse-delay)
  :hook (emacs-startup . pulsar-global-mode))

;; Pulse modified region
(when emacs/>=29p
  (use-package goggles
    :diminish
    :hook (prog-mode text-mode conf-mode)))

(provide 'init-highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-highlight.el ends here
