;; init-tools.el --- Setup useful tools.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package which-key
  :diminish
  :bind ("C-h M-m" . which-key-show-major-mode)
  :hook (after-init . which-key-mode)
  :init
  (setq which-key-idle-delay 0.2)
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix " ")

  ;; Needed to avoid nil variable error before update to recent which-key
  (defvar which-key-replacement-alist nil)
  ;; Reset to the default or customized value before adding our values in order
  ;; to make this initialization code idempotent.
  (custom-reevaluate-setting 'which-key-replacement-alist)
  ;; Replace rules for better naming of functions
  (let ((new-descriptions
         ;; being higher in this list means the replacement is applied later
         '(
           ("petmacs/\\(.+\\)" . "\\1")
           ("petmacs/toggle-\\(.+\\)" . "\\1")
           ("avy-goto-word-or-subword-1" . "avy word")
           ("shell-command" . "shell cmd")
           ("universal-argument" . "universal arg")
           ("er/expand-region" . "expand region")
	   ("counsel-projectile-rg". "project rg")
           ("evil-lisp-state-\\(.+\\)" . "\\1")
           ("helm-mini\\|ivy-switch-buffer" . "list-buffers"))))
    (dolist (nd new-descriptions)
      ;; ensure the target matches the whole string
      (push (cons (cons nil (concat "\\`" (car nd) "\\'")) (cons nil (cdr nd)))
            which-key-replacement-alist))))

(when (childframe-workable-p)
  (use-package which-key-posframe
    :diminish
    :functions posframe-poshandler-frame-center-near-bottom
    :custom-face
    (which-key-posframe ((t (:inherit tooltip))))
    (which-key-posframe-border ((t (:background ,(face-foreground 'font-lock-comment-face nil t)))))
    :init
    (setq which-key-posframe-border-width 3
          which-key-posframe-poshandler #'posframe-poshandler-frame-center-near-bottom
          which-key-posframe-parameters '((left-fringe . 8)
                                          (right-fringe . 8)))
    (which-key-posframe-mode 1)
    :config
    (with-no-warnings
      (defun my-which-key-posframe--show-buffer (act-popup-dim)
        "Show which-key buffer when popup type is posframe.
Argument ACT-POPUP-DIM includes the dimension, (height . width)
of the buffer text to be displayed in the popup"
        (when (posframe-workable-p)
          (with-current-buffer (get-buffer-create which-key-buffer-name)
            (let ((inhibit-read-only t))
              (goto-char (point-min))
              (insert (propertize "\n" 'face '(:height 0.3)))
              (goto-char (point-max))
              (insert (propertize "\n\n\n" 'face '(:height 0.3)))))
          (posframe-show which-key--buffer
		         :font which-key-posframe-font
		         :position (point)
		         :poshandler which-key-posframe-poshandler
		         :background-color (face-attribute 'which-key-posframe :background nil t)
		         :foreground-color (face-attribute 'which-key-posframe :foreground nil t)
		         :height (1+ (car act-popup-dim))
		         :width (1+ (cdr act-popup-dim))
		         :internal-border-width which-key-posframe-border-width
		         :internal-border-color (face-attribute 'which-key-posframe-border :background nil t)
		         :override-parameters which-key-posframe-parameters)))
      (advice-add #'which-key-posframe--show-buffer :override #'my-which-key-posframe--show-buffer))

    (add-hook 'after-load-theme-hook
              (lambda ()
                (custom-set-faces
                 '(which-key-posframe ((t (:inherit tooltip))))
                 `(which-key-posframe-border ((t (:background ,(face-foreground 'font-lock-comment-face nil t))))))))))

(use-package expand-region
  :init
  (setq expand-region-contract-fast-key "V"
        expand-region-reset-fast-key "r"))

(use-package editorconfig
  :diminish
  :hook (after-init . editorconfig-mode))

;; Fast search tool `ripgrep'
(use-package rg
  :defines projectile-command-map
  :hook (after-init . rg-enable-default-bindings)
  :bind (:map rg-global-map
         ("c" . rg-dwim-current-dir)
         ("f" . rg-dwim-current-file)
         ("m" . rg-menu))
  :init (setq rg-group-result t
              rg-show-columns t)
  :config
  (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)

  (with-eval-after-load 'projectile
    (bind-key "s R" #'rg-project projectile-command-map)))

(use-package avy
  :defer nil
  :commands (avy-with)
  :init
  (setq avy-timeout-seconds 0.0))

;; Kill text between the point and the character CHAR
(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

;; Quickly follow links
(use-package ace-link
  :defines (org-mode-map
            gnus-summary-mode-map
            gnus-article-mode-map
            ert-results-mode-map
            paradox-menu-mode-map
            elfeed-show-mode-map)
  :bind ("M-o" . ace-link-addr)
  :hook (after-init . ace-link-setup-default)
  :config
  (with-eval-after-load 'org
    (bind-key "M-o" #'ace-link-org org-mode-map))

  (with-eval-after-load 'gnus
    (bind-keys
     :map gnus-summary-mode-map
     ("M-o" . ace-link-gnus)
     :map gnus-article-mode-map
     ("M-o" . ace-link-gnus)))

  (with-eval-after-load 'ert
    (bind-key "o" #'ace-link-help ert-results-mode-map))

  (bind-keys
   :map package-menu-mode-map
   ("o" . ace-link-help)
   :map process-menu-mode-map
   ("o" . ace-link-help))
  (with-eval-after-load 'paradox
    (bind-key "o" #'ace-link-help paradox-menu-mode-map))

  (with-eval-after-load 'elfeed
    (bind-key "o" #'ace-link elfeed-show-mode-map)))

;; Jump to Chinese characters
(use-package ace-pinyin
  :diminish
  :hook (after-init . ace-pinyin-global-mode))

(use-package quickrun
  :custom (quickrun-focus-p nil)
  :bind (("C-c x" . quickrun)))

;; M-x rmsbolt-starter to see assembly code
(use-package rmsbolt
  :custom
  (rmsbolt-asm-format nil)
  (rmsbolt-default-directory "/tmp"))

(use-package pyim
  :init
  (setq default-input-method "pyim"
	pyim-page-length 7
	pyim-punctuation-translate-p '(auto yes no) ;中文使用全角标点，英文使用半角标点
	)
  (if (posframe-workable-p)
      (setq pyim-page-tooltip 'posframe)
    (setq pyim-page-tooltip 'popup))

  :config
  (pyim-default-scheme 'quanpin)

  ;; 金手指设置，可以将光标处的编码，比如：拼音字符串，转换为中文。
  (global-set-key (kbd "C-j") 'pyim-convert-string-at-point)
  (global-set-key (kbd "C-\\") 'toggle-input-method)

  ;; 按 "C-<return>" 将光标前的 regexp 转换为可以搜索中文的 regexp.
  (define-key minibuffer-local-map (kbd "C-<return>") 'pyim-cregexp-convert-at-point)

  (define-key pyim-mode-map "." 'pyim-page-next-page)
  (define-key pyim-mode-map "," 'pyim-page-previous-page)
  ;; 用 “;” 来选择第二个候选词
  (define-key pyim-mode-map ";"
    (lambda ()
      (interactive)
      (pyim-select-word-by-number 2)))


  (pyim-extra-dicts-add-dict
   `(:name "Great-dict-private"
     :file, (expand-file-name "resources/dicts/pyim-greatdict.pyim.gz" user-emacs-directory)
     :coding utf-8-unix
     :dict-type pinyin-dict
     :elpa t))
  )

;; Cross-referencing commands
(use-package xref
  :ensure nil
  :init
  (when (and (boundp 'xref-search-program) (executable-find "rg"))
    (setq xref-search-program 'ripgrep))

  (with-no-warnings
    (if emacs/>=28p
        (setq xref-show-xrefs-function #'xref-show-definitions-completing-read
              xref-show-definitions-function #'xref-show-definitions-completing-read)
      ;; Select from xref candidates with Ivy
      (use-package ivy-xref
        :after ivy
        :init
        (when emacs/>=27p
          (setq xref-show-definitions-function #'ivy-xref-show-defs))
        (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)))))

;; Hungry deletion
;; (use-package hungry-delete
;;   :diminish
;;   :hook (after-init . global-hungry-delete-mode)
;;   :init (setq hungry-delete-except-modes
;;               '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode)))

(use-package centered-cursor-mode)

(use-package general)
(use-package restart-emacs)
(use-package focus)                     ; Focus on the current region
(use-package carbon-now-sh)
(use-package daemons)                   ; system services/daemons
;;   :after lsp-java)
(use-package transwin)


(provide 'init-tools)

;;; init-tools.el ends here
