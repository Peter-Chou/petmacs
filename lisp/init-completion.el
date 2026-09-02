;;; init-completion.el --- Initialize completion configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Modern completion configuration.
;;

;;; Code:

;; Suppress warnings
(eval-when-compile
  (require 'init-custom)
  (require 'init-funcs))

(when emacs/>=28p
  ;; Optionally use the `orderless' completion style.
  (use-package orderless
    :custom
    (completion-styles '(orderless basic))
    (completion-category-defaults nil)
    (completion-category-overrides '((file (styles basic partial-completion))))
    (orderless-component-separator #'orderless-escapable-split-on-space))

  ;; Support Pinyin
  (use-package pinyinlib
    :after orderless
    :functions orderless-regexp
    :autoload pinyinlib-build-regexp-string
    :init
    (defun orderless-regexp-pinyin (str)
      "Match COMPONENT as a pinyin regex."
      (orderless-regexp (pinyinlib-build-regexp-string str)))
    (add-to-list 'orderless-matching-styles 'orderless-regexp-pinyin))

  ;; VERTical Interactive COmpletion
  (use-package vertico
    :custom (vertico-count 15)
    :bind (:map vertico-map
           ("RET" . vertico-directory-enter)
           ("DEL" . vertico-directory-delete-char)
           ("M-DEL" . vertico-directory-delete-word))
    :hook ((after-init . vertico-mode)
           (rfn-eshadow-update-overlay . vertico-directory-tidy)))

  ;; Display vertico in the child frame
  (use-package vertico-posframe
    :functions (childframe-completion-workable-p
                posframe-poshandler-frame-center-near-bottom)
    :commands vertico-posframe-mode
    :hook ((server-after-make-frame vertico-mode)
           .
           (lambda ()
             "Handle vertico child frame."
             (and (childframe-completion-workable-p)
                  (vertico-posframe-mode 1))))
    :init (setq vertico-posframe-poshandler
                #'posframe-poshandler-frame-center-near-bottom
                vertico-posframe-parameters
                '((left-fringe  . 8)
                  (right-fringe . 8))))

  ;; Enrich existing commands with completion annotations
  (use-package marginalia
    :pin melpa-stable
    :hook (after-init . marginalia-mode))

  ;; Add icons to completion candidates
  (use-package nerd-icons-completion
    :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

  ;; Consulting completing-read
  (use-package consult
    :defines (xref-show-xrefs-function xref-show-definitions-function shr-color-html-colors-alist)
    :autoload (consult-register-format consult-register-window consult-xref)
    :autoload (consult--read consult--customize-put consult--grep)
    :bind (([remap Info-search]        . consult-info)
           ([remap isearch-forward]    . consult-line)
           ([remap recentf-open-files] . consult-recent-file)
           :map isearch-mode-map
           ("M-e"     . consult-isearch-history)      ;; orig. isearch-edit-string
           ("M-s e"   . consult-isearch-history)      ;; orig. isearch-edit-string
           ("M-s l"   . consult-line)                 ;; needed by consult-line to detect isearch
           ("M-s L"   . consult-line-multi)           ;; needed by consult-line to detect isearch

           ;; Minibuffer history
           :map minibuffer-local-map
           ("M-s" . consult-history)                  ;; orig. next-matching-history-element
           ("M-r" . consult-history))                 ;; orig. previous-matching-history-element
    :init
    ;; Optionally configure the register formatting. This improves the register
    ;; preview for `consult-register', `consult-register-load',
    ;; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0.5
          register-preview-function #'consult-register-format)

    ;; Optionally tweak the register preview window.
    ;; This adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)

    ;; Use Consult to select xref locations with preview
    (with-eval-after-load 'xref
      (setq xref-show-xrefs-function #'consult-xref
            xref-show-definitions-function #'consult-xref))

    ;; No longer preloaded in Emacs 28.
    (autoload 'list-colors-duplicates "facemenu")
    ;; No preloaded in consult.el
    (autoload 'consult--read "consult")
    :config
    ;; Optionally configure preview. The default value
    ;; is 'any, such that any key triggers the preview.
    ;; (setq consult-preview-key 'any)
    ;; (setq consult-preview-key "M-.")
    ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
    (setq consult-preview-key nil)

    ;; For some commands and buffer sources it is useful to configure the
    ;; :preview-key on a per-command basis using the `consult-customize' macro.
    (consult-customize
     consult-goto-line :preview-key 'any
     consult-theme :preview-key '("M-." :debounce 0.5 "<up>" "<down>")

     consult-buffer consult-recent-file
     consult-source-recent-file consult-source-project-recent-file
     :preview-key '("M-.")

     consult-man consult-bookmark consult-xref
     consult-source-bookmark consult-source-file-register
     :preview-key '(:debounce 0.4 any)

     consult-line consult-line-multi
     consult-ripgrep consult-git-grep consult-grep
     :initial (selected-region-or-symbol-at-point)
     :preview-key 'any)

    ;; Optionally configure the narrowing key.
    ;; Both < and C-+ work reasonably well.
    (setq consult-narrow-key "<"  ;; "C-+"
          consult-project-function (lambda (_) (projectile-project-root)))

    ;; Select initial texts
    ;; It's useful in `consult-grep' and similar commands
    (defun my/consult--read (fn &rest args)
      "Select initial texts in `consult--read'."
      (minibuffer-with-setup-hook
          (lambda ()
            "Select initial texts."
            (set-mark (point-max))
            (goto-char (minibuffer-prompt-end)))
        (apply fn args)))
    (advice-add #'consult--read :around #'my/consult--read)

    ;;
    ;; More utilities: list colors
    ;;
    (defvar consult-colors-history nil
      "History for `consult-colors-emacs' and `consult-colors-web'.")

    ;; No longer preloaded in Emacs 28.
    (autoload 'list-colors-duplicates "facemenu")

    (defun consult-colors-emacs (color)
      "Show a list of all supported colors for a particular frame.

You can insert the name (default), or insert or kill the hexadecimal or RGB
value of the selected COLOR."
      (interactive
       (list (consult--read (list-colors-duplicates (defined-colors))
                            :prompt "Emacs color: "
                            :require-match t
                            :category 'color
                            :history '(:input consult-colors-history))))
      (insert color))

    ;; Adapted from counsel.el to get web colors.
    (defun consult-colors--web-list nil
      "Return list of CSS colors for `counsult-colors-web'."
      (require 'shr-color)
      (sort (mapcar #'downcase (mapcar #'car shr-color-html-colors-alist)) #'string-lessp))

    (defun consult-colors-web (color)
      "Show a list of all CSS colors.\

You can insert the name (default), or insert or kill the hexadecimal or RGB
value of the selected COLOR."
      (interactive
       (list (consult--read (consult-colors--web-list)
                            :prompt "Color: "
                            :require-match t
                            :category 'color
                            :history '(:input consult-colors-history))))
      (insert color)))

  (use-package consult-dir)
  (use-package consult-yasnippet)
  ;; (use-package consult-flyspell)

  (use-package embark
    :commands embark-prefix-help-command
    :bind (("C-." . embark-act)         ;; pick some comfortable binding
           ("M-." . embark-dwim)        ;; good alternative: M-.
           ("C-h B" . embark-bindings)  ;; alternative for `describe-bindings'
           ([remap describe-bindings] . embark-bindings)
           :map minibuffer-local-map
           ("M-." . my/embark-preview))
    :init
    ;; Optionally replace the key help with a completing-read interface
    (setq prefix-help-command #'embark-prefix-help-command)
    :config
    ;; Manual preview for non-Consult commands using Embark
    (defun my/embark-preview ()
      "Previews candidate in vertico buffer, unless it's a consult command."
      (interactive)
      (unless (bound-and-true-p consult--preview-function)
        (save-selected-window
          (let ((embark-quit-after-action nil))
            (embark-dwim)))))

    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none))))

    (with-no-warnings
      (with-eval-after-load 'which-key
        (defun embark-which-key-indicator ()
          "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
          (lambda (&optional keymap targets prefix)
            (if (null keymap)
                (which-key--hide-popup-ignore-command)
              (which-key--show-keymap
               (if (eq (plist-get (car targets) :type) 'embark-become)
                   "Become"
                 (format "Act on %s '%s'%s"
                         (plist-get (car targets) :type)
                         (embark--truncate-target (plist-get (car targets) :target))
                         (if (cdr targets) "…" "")))
               (if prefix
                   (pcase (lookup-key keymap prefix 'accept-default)
                     ((and (pred keymapp) km) km)
                     (_ (key-binding prefix 'accept-default)))
                 keymap)
               nil nil t (lambda (binding)
                           (not (string-suffix-p "-argument" (cdr binding))))))))

        (setq embark-indicators
              '(embark-which-key-indicator
                embark-highlight-indicator
                embark-isearch-highlight-indicator))

        (defun embark-hide-which-key-indicator (fn &rest args)
          "Hide the which-key indicator immediately when using the completing-read prompter."
          (which-key--hide-popup-ignore-command)
          (let ((embark-indicators
                 (remq #'embark-which-key-indicator embark-indicators)))
            (apply fn args)))

        (advice-add #'embark-completing-read-prompter
                    :around #'embark-hide-which-key-indicator))))

  (use-package embark-consult)

  ;; Auto completion
  (use-package corfu
    :autoload (corfu-quit consult-completion-in-region)
    :functions (corfu-move-to-minibuffer)
    :custom
    (corfu-auto t)
    (corfu-auto-prefix 2)
    (corfu-count 12)
    (corfu-preview-current nil)
    (corfu-on-exact-match nil)
    (corfu-auto-delay 0.2)
    (corfu-popupinfo-delay '(0.4 . 0.2))
    (global-corfu-modes '((not erc-mode
                               circe-mode
                               help-mode
                               gud-mode)
                          t))
    :custom-face
    (corfu-border ((t (:inherit region :background unspecified))))
    :bind ("M-/" . completion-at-point)
    :hook ((after-init . global-corfu-mode)
           (global-corfu-mode . corfu-popupinfo-mode)
           (global-corfu-mode . corfu-history-mode))
    :config
    ;;Quit completion before saving
    (add-hook 'before-save-hook #'corfu-quit)

    ;; Move completions to minibuffer
    (defun corfu-move-to-minibuffer ()
      (interactive)
      (pcase completion-in-region--data
        (`(,beg ,end ,table ,pred ,extras)
         (let ((completion-extra-properties extras)
               completion-cycle-threshold completion-cycling)
           (consult-completion-in-region beg end table pred)))))
    (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
    (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer))

  ;; (unless (childframe-workable-p)
  ;;   (use-package corfu-terminal
  ;;     :functions childframe-workable-p
  ;;     :hook (global-corfu-mode . corfu-terminal-mode)))

  ;; A few more useful configurations...
  (use-package emacs
    :custom
    ;; TAB cycle if there are only few candidates
    ;; (completion-cycle-threshold 3)

    ;; Enable indentation+completion using the TAB key.
    ;; `completion-at-point' is often bound to M-TAB.
    (tab-always-indent 'complete)

    ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
    ;; try `cape-dict'.
    (text-mode-ispell-word-completion nil)

    ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
    ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
    ;; setting is useful beyond Corfu.
    (read-extended-command-predicate #'command-completion-default-include-p))

  (use-package nerd-icons-corfu
    :autoload nerd-icons-corfu-formatter
    :after corfu
    :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

  ;; Add extensions
  (use-package cape
    :commands (cape-file cape-elisp-block cape-keyword)
    :autoload (cape-wrap-noninterruptible cape-wrap-nonexclusive cape-wrap-buster)
    :autoload (cape-wrap-silent)
    :init
    ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-elisp-block)
    (add-to-list 'completion-at-point-functions #'cape-keyword)
    ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)

    ;; Make these capfs composable.
    (advice-add 'comint-completion-at-point :around #'cape-wrap-nonexclusive)
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive)))

(provide 'init-completion)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-completion.el ends here
