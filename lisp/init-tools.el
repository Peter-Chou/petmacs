;; init-tools.el --- Better default configurations.	-*- lexical-binding: t -*-

(use-package posframe)

(use-package general)
(use-package bind-map)
(use-package bind-key)

(use-package which-key
  :diminish
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

(use-package display-fill-column-indicator
  :ensure nil
  :hook
  (prog-mode . display-fill-column-indicator-mode)
  :init
  (setq-default fill-column  80)
  (setq display-fill-column-indicator-character "|"))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package visual-regexp
  :defer
  :commands (vr/replace vr/query-replace))


(use-package visual-regexp-steroids
  :defer
  :commands (vr/select-replace vr/select-query-replace))


(use-package org-super-agenda)

(use-package protobuf-mode
  :hook (protobuf-mode . disable-curly-bracket-electric-pair))

(use-package olivetti
  :init
  (setq olivetti-body-width nil)
  :config
  (defun distraction-free ()
    "Distraction-free writing environment"
    (interactive)
    (if (equal olivetti-mode nil)
        (olivetti-mode t)
      (progn
        (olivetti-mode 0)))))

(use-package editorconfig
  :diminish
  :hook (after-init . editorconfig-mode))

(use-package toggle-one-window
  :quelpa
  (toggle-one-window :fetcher github
  		             :repo "manateelazycat/toggle-one-window"
  		             :files ("*.el"))
  :commands (toggle-one-window))

(use-package ace-window
  :custom-face
  (aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 2.0))))
  (aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 1.0))))
  (aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
  :bind ([remap other-window] . ace-window)
  :init
  ;; (setq aw-scope 'frame) ;; jump only in current frame
  (setq aw-minibuffer-flag t))

(use-package pyim
  :init
  (setq default-input-method "pyim"
	    pyim-page-length 7
	    pyim-punctuation-translate-p '(auto yes no) ;中文使用全角标点，英文使用半角标点
	    )
  (if (posframe-workable-p)
      (setq pyim-page-tooltip 'posframe)
    (setq pyim-page-tooltip 'popup))
  ;; (setq pyim-page-tooltip 'popup)

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

(use-package toggle-one-window
  :quelpa
  (toggle-one-window :fetcher github
  		             :repo "manateelazycat/toggle-one-window"
  		             :files ("*.el"))
  :commands (toggle-one-window))

(use-package hydra
  :functions hydra-frame-window/body
  :config
  (defhydra hydra-frame-window (:color pink :hint nil)
    "
^Frame^                 ^Window^      ^Window Size^^^^     ^Text Zoom^
^^----------------------^^------------^^----------^^^^-----^^---------------         (__)
_0_: delete             _t_oggle        ^ ^ _k_ ^ ^            _+_                   (oo)
_1_: delete others      _s_wap          _h_ ^+^ _l_            _=_             /------\\/
_2_: new                _d_elete        ^ ^ _j_ ^ ^            _-_            / |    ||
_F_ullscreen            _o_ther         _b_alance^^^^          ^ ^         *  /\\-----/\\  ~~  C-c w/C-x o w
"
    ("0" delete-frame :exit t)
    ("1" delete-other-frames :exit t)
    ("2" make-frame  :exit t)
    ("b" balance-windows)
    ("s" ace-swap-window)
    ("F" toggle-frame-fullscreen)
    ("t" toggle-window-split)
    ("d" ace-delete-window :exit t)
    ("o" ace-window :exit t)
    ("-" text-scale-decrease)
    ("=" (text-scale-increase 0))
    ("+" text-scale-increase)
    ("h" shrink-window-horizontally)
    ("k" shrink-window)
    ("j" enlarge-window)
    ("l" enlarge-window-horizontally)
    ("q" nil "quit"))
  )

(use-package centered-cursor-mode)
(use-package restart-emacs)
(use-package focus)                     ; Focus on the current region
(use-package carbon-now-sh)

(provide 'init-tools)
