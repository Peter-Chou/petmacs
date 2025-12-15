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

(use-package orderless
  :demand t
  ;; ...otherwise find-file gets different highlighting than other commands
  ;; (set-face-attribute 'completions-first-difference nil :inherit nil)
  :config
  (defvar +orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
      (?! . orderless-without-literal)
      (?`. orderless-initialism)
      (?= . orderless-literal)
      (?~ . orderless-flex)))

  (defun +orderless-dispatch (pattern index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x100000-\x10FFFD]*$")))
     ;; File extensions
     ((and
       ;; Completing filename or eshell
       (or minibuffer-completing-file-name
           (derived-mode-p 'eshell-mode))
       ;; File extension
       (string-match-p "\\`\\.." pattern))
      `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x100000-\x10FFFD]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Prefix and suffix
     ((if-let (x (assq (aref pattern 0) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 1))
        (when-let (x (assq (aref pattern (1- (length pattern))) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 0 -1)))))))

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

  (setq
   ;; use & to segment parts of candicate
   ;; orderless-component-separator "[ &]"
   ;; completion-styles '(basic substring partial-completion orderless flex)
   completion-styles '(orderless partial-completion basic)
   completion-category-defaults nil
   completion-category-overrides '((file (styles orderless partial-completion)) ;; partial-completion is tried first
                                   (command (styles +orderless-with-initialism))
                                   (variable (styles +orderless-with-initialism))
                                   (symbol (styles +orderless-with-initialism)))
   orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
   orderless-style-dispatchers '(+orderless-dispatch))
  )

;; Support
(use-package pinyinlib
  :after orderless
  :functions orderless-regexp
  :autoload pinyinlib-build-regexp-string
  :init
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

(use-package vertico
  :bind (:map vertico-map
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word)
         :map minibuffer-mode-map
         ("M-n" . vertico-next)
         ("M-p" . vertico-previous))
  :hook ((after-init . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy))
  :init
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t
        vertico-count 12
        vertico-cycle t)

  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  :config
  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

(when (childframe-completion-workable-p)
  (use-package vertico-posframe
    :functions posframe-poshandler-frame-center-near-bottom
    :hook (vertico-mode . vertico-posframe-mode)
    :init (setq vertico-posframe-poshandler
                #'posframe-poshandler-frame-center-near-bottom
                vertico-posframe-parameters
                '((left-fringe  . 8)
                  (right-fringe . 8)))))

(use-package consult
  :defines (xref-show-xrefs-function xref-show-definitions-function)
  :defines shr-color-html-colors-alist
  :autoload (consult-register-format consult-register-window consult-xref)
  :autoload (consult--read consult--customize-put)
  :commands (consult-narrow-help)
  :functions (list-colors-duplicates consult-colors--web-list)
  :bind (("C-s"   . consult-line)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ("C-c r" . consult-ripgrep)

         ([remap Info-search]        . consult-info)
         ([remap imenu]              . consult-imenu)
         ([remap isearch-forward]    . consult-line)
         ([remap recentf-open-files] . consult-recent-file)
         )
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  (if sys/win32p
      (progn
        (add-to-list 'process-coding-system-alist '("es" gbk . gbk))
        (add-to-list 'process-coding-system-alist '("explorer" gbk . gbk))
        (setq consult-locate-args (encode-coding-string "es.exe -i -p -r" 'gbk))))

  ;; Use Consult to select xref locations with preview
  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))

  :config
  (setq ;; consult-project-root-function #'doom-project-root
   consult-narrow-key "<"
   consult-project-function (lambda (_) (projectile-project-root))
   consult-line-numbers-widen t
   consult-line-start-from-top t
   consult-async-min-input 2
   consult-async-refresh-delay  0.15
   consult-async-input-throttle 0.2
   consult-async-input-debounce 0.1)

  (consult-customize
   ;; consult-ripgrep consult-git-grep consult-grep
   ;; consult-bookmark consult-recent-file consult-xref
   ;; :preview-key "M-."

   consult-line consult-line-multi
   :preview-key 'any
   consult-buffer consult-recent-file consult-theme :preview-key '(:debounce 1.0 any)
   consult-goto-line :preview-key '(:debounce 0.5 any)
   consult-ripgrep consult-git-grep consult-grep
   :initial (selected-region-or-symbol-at-point)
   :preview-key '(:debounce 0.5 any)
   consult-theme
   :preview-key (list :debounce 0.5 'any))

  (advice-add #'multi-occur :override #'consult-multi-occur))

(use-package consult-dir
  :bind (([remap list-directory] . consult-dir)))

(use-package consult-yasnippet
  :after (consult yasnippet))

(use-package embark
  :commands embark-prefix-help-command
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)  ;; alternative for `describe-bindings'
   ;; ([remap xref-find-definitions] . embark-dwim)
   ([remap describe-bindings] . embark-bindings)
   )
  :init
  (setq which-key-use-C-h-commands nil
        embark-help-key "?"
        ;; press C-h after a prefix key, it shows all the possible key bindings and let you choose what you want
        prefix-help-command #'embark-prefix-help-command
        embark-verbose-indicator-display-action
        '((display-buffer-at-bottom)
          (window-parameters (mode-line-format . none))
          (window-height . fit-window-to-buffer)))

  :config
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
                  :around #'embark-hide-which-key-indicator)))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (define-key evil-normal-state-map (kbd "C-.") 'embark-act)
  (define-key evil-normal-state-map (kbd "M-.") 'embark-dwim)
  ;; list all the keybindings in this buffer
  (global-set-key (kbd "C-h B") 'embark-bindings)
  (define-key embark-file-map (kbd "E") #'consult-directory-externally)
  (define-key embark-file-map (kbd "U") #'consult-snv-unlock))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package embark-consult
  :after (embark consult)
  :bind (:map minibuffer-mode-map
         ("C-c C-o" . embark-export))
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :autoload (corfu-quit consult-completion-in-region)
  :functions (corfu-move-to-minibuffer)
  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  :bind (:map corfu-map
         ("C-M-m" . corfu-move-to-minibuffer)
         ("M-P" . corfu-popupinfo-scroll-down) ;; corfu-next
         ("M-N" . corfu-popupinfo-scroll-up) ;; corfu-previous
         )
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode))
  :init
  (setq corfu-auto t
        corfu-cycle t
        corfu-auto-prefix 2
        corfu-auto-delay 0.2
        corfu-min-width 80
        corfu-max-width 100
        corfu-popupinfo-delay '(0.4 . 0.2)
        corfu-popupinfo-max-width 120
        corfu-popupinfo-max-height 40
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match 'separator
        corfu-preselect 'first
        corfu-preview-current nil
        corfu-on-exact-match nil)
  (require 'corfu)
  (when (> (frame-pixel-width) 3000) (custom-set-faces '(corfu-default ((t (:height 1.3))))))

  (defun corfu-beginning-of-prompt ()
    "Move to beginning of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (car completion-in-region--data)))

  (defun corfu-end-of-prompt ()
    "Move to end of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (cadr completion-in-region--data)))

  (define-key corfu-map [remap move-beginning-of-line] #'corfu-beginning-of-prompt)
  (define-key corfu-map [remap move-end-of-line] #'corfu-end-of-prompt)
  :config
  (with-eval-after-load 'eglot
    (defun petmacs/eglot-capf-setup ()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless)) ;; Configure orderless
      (setq-local completion-at-point-functions (list
                                                 #'eglot-completion-at-point
                                                 #'cape-file
                                                 #'yasnippet-capf
    		                                     #'cape-dabbrev)))
    (add-hook 'eglot-managed-mode-hook #'petmacs/eglot-capf-setup))
  ;;Quit completion before saving
  (add-hook 'before-save-hook #'corfu-quit))

(use-package nerd-icons-corfu
  :autoload nerd-icons-corfu-formatter
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Use dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :init
  (setq dabbrev-upcase-means-case-search t
        dabbrev-check-all-buffers nil
        dabbrev-check-other-buffers t
        dabbrev-friend-buffer-function 'dabbrev--same-major-mode-p
        dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package cape
  :commands (cape-file cape-elisp-block cape-keyword)
  :autoload (cape-wrap-noninterruptible cape-wrap-nonexclusive cape-wrap-buster)
  :autoload (cape-wrap-silent)
  :init (setq cape-dabbrev-check-other-buffers 'some
              ;; cape-dabbrev-check-other-buffers nil
              cape-dabbrev-min-length 2)
  :config
  ;; 默认用这三个补全后端
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)

  (advice-add 'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive)

  ;; Sanitize the `pcomplete-completions-at-point' Capf.  The Capf has undesired
  ;; side effects on Emacs 28.  These advices are not needed on Emacs 29 and newer.
  (unless emacs/>=29p
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)))

(unless (or (display-graphic-p) (featurep 'tty-child-frames))
  (use-package corfu-terminal
    :hook (global-corfu-mode . corfu-terminal-mode)))

(provide 'init-completion)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-completion.el ends here
