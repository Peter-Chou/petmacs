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

(use-package rg
  :hook (after-init . rg-enable-default-bindings)
  :config
  (setq rg-group-result t)
  (setq rg-show-columns t)

  (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)

  (with-eval-after-load 'projectile
    (defalias 'projectile-ripgrep 'rg-project)
    (bind-key "s R" #'rg-project projectile-command-map)))

(use-package avy
  :defer nil
  :commands (avy-with)
  :init
  (setq avy-timeout-seconds 0.0))

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
  (require 'pyim-tsinghua-dict)
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

  (pyim-tsinghua-dict-enable)
  )

(use-package centered-cursor-mode)

(use-package general)
(use-package restart-emacs)
(use-package focus)                     ; Focus on the current region
(use-package carbon-now-sh)
(use-package daemons)                   ; system services/daemons
(use-package diffview)                  ; side-by-side diff view
(use-package esup)                      ; Emacs startup profiler
;;   :after lsp-java)
(use-package transwin)


(provide 'init-tools)

;;; init-tools.el ends here
