;; -*- lexical-binding: t no-byte-compile: t -*-

(require 'init-custom)

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
        vertico-resize petmacs-enable-mini-frame
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
  ;; fine tuning the Vertico display per command or completion
  (use-package vertico-multiform
    :ensure nil
    :hook (after-init . vertico-multiform-mode)
    :init (setq vertico-multiform-commands
                '((consult-line (:not posframe))
                  (consult-ripgrep (:not posframe))
                  (consult-grep (:not posframe))
                  (consult-imenu (:not posframe))
                  (xref-find-definitions (:not posframe))
                  (t posframe))))
  (use-package vertico-posframe
    :hook (vertico-mode . vertico-posframe-mode)
    :init (setq vertico-posframe-poshandler
                #'posframe-poshandler-frame-center-near-bottom
                vertico-posframe-parameters
                '((left-fringe  . 8)
                  (right-fringe . 8)))))

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

(use-package consult
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
  ;; :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
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
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   :preview-key "M-.")

  (consult-customize
   consult-theme
   :preview-key (list :debounce 0.5 'any))

  (advice-add #'multi-occur :override #'consult-multi-occur))

(use-package consult-dir
  :bind (([remap list-directory] . consult-dir)))

(use-package consult-yasnippet
  :after (consult yasnippet))

(use-package embark
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
              :around #'embark-hide-which-key-indicator)

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

;; edit the text in the grep buffer after typing C-c C-p
(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

;; Support
(use-package pinyinlib
  :after orderless
  :autoload pinyinlib-build-regexp-string
  :init
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

(provide 'init-consult)
