;; init-lsp.el --- Better default configurations.	-*- lexical-binding: t -*-

(require 'init-custom)

(use-package kind-all-the-icons
  :load-path (lambda () (expand-file-name "site-lisp/kind-all-the-icons" user-emacs-directory))
  :init
  (require 'kind-all-the-icons)
  (if (display-graphic-p)
      (add-to-list 'corfu-margin-formatters #'kind-all-the-icons-margin-formatter)))


;; optimize lsp-mode
(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024)
      lsp-use-plists t
      lsp-log-io nil)

;; sudo apt-get install libjansson-dev
(use-package lsp-mode
  :hook (((c-mode c++-mode cuda-mode) . (lambda ()
					                      (lsp-deferred)))
         (scala-mode . (lambda ()
			             (require 'lsp-metals)
			             (lsp-deferred)))
         ((markdown-mode yaml-mode) . lsp-deferred)
         (go-mode . lsp-deferred)
         (java-mode . lsp-deferred)
         (lsp-mode . (lambda ()
                       ;; Integrate `which-key'
                       (lsp-enable-which-key-integration)

                       ;; Format and organize imports
                       (unless (apply #'derived-mode-p petmacs-lsp-format-on-save-ignore-modes)
                         (add-hook 'before-save-hook #'lsp-format-buffer t t)
                         (add-hook 'before-save-hook #'lsp-organize-imports t t)))))
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-keep-workspace-alive nil
        lsp-signature-auto-activate nil
        lsp-modeline-code-actions-enable nil
        ;; lsp-modeline-code-actions-enable t
        lsp-modeline-diagnostics-enable nil
        ;; lsp-modeline-diagnostics-enable t
	    lsp-modeline-workspace-status-enable nil
	    lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-segments '(symbols)

        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        ;; lsp-enable-folding t
        lsp-enable-symbol-highlighting nil
        lsp-enable-text-document-color nil

        lsp-enable-indentation nil
	    lsp-enable-on-type-formatting nil
        lsp-auto-guess-root nil
        ;; lsp-prefer-capf t

	    lsp-enable-on-type-formatting nil)
  :config
  (with-no-warnings
    ;; Enable `lsp-mode' in sh/bash/zsh
    (defun my-lsp-bash-check-sh-shell (&rest _)
      (and (eq major-mode 'sh-mode)
           (memq sh-shell '(sh bash zsh))))
    (advice-add #'lsp-bash-check-sh-shell :override #'my-lsp-bash-check-sh-shell)

    ;; Only display icons in GUI
    (defun my-lsp-icons-get-symbol-kind (fn &rest args)
      (when (and petmacs-icon (display-graphic-p))
	    (apply fn args)))
    (advice-add #'lsp-icons-get-by-symbol-kind :around #'my-lsp-icons-get-symbol-kind)))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :init (setq lsp-ui-sideline-show-diagnostics nil
	          lsp-ui-sideline-enable nil
	          lsp-ui-sideline-show-code-actions nil
              lsp-ui-sideline-ignore-duplicate t
              lsp-idle-delay 0.25
	          lsp-ui-doc-enable nil
	          lsp-ui-doc-delay 0.1
	          lsp-ui-doc-border (face-foreground 'font-lock-comment-face nil t)
              lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
				                    ,(face-foreground 'font-lock-string-face)
				                    ,(face-foreground 'font-lock-constant-face)
				                    ,(face-foreground 'font-lock-variable-name-face)))
  )

(use-package lsp-treemacs
  :after lsp-mode
  :bind (:map lsp-mode-map
         ("C-<f8>" . lsp-treemacs-errors-list)
         ("M-<f8>" . lsp-treemacs-symbols)
         ("s-<f8>" . lsp-treemacs-java-deps-list))
  :init (lsp-treemacs-sync-mode 1)
  (setq
   lsp-treemacs-deps-position-params
   `((side . right)
     (slot . 1)
     (window-width . 32))
   lsp-treemacs-symbols-position-params
   `((side . right)
     (slot . 2)
     (window-width . 32)))
  :config
  (with-eval-after-load 'ace-window
    (when (boundp 'aw-ignored-buffers)
      (push 'lsp-treemacs-symbols-mode aw-ignored-buffers)
      (push 'lsp-treemacs-java-deps-mode aw-ignored-buffers))))

(when sys/macp
  (use-package lsp-sourcekit
    :init (setq lsp-sourcekit-executable
                "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp")))


;;; python
(use-package lsp-pyright
  :preface
  ;; Use yapf to format
  (defun lsp-pyright-format-buffer ()
	(interactive)
	(when (and (executable-find "yapf") buffer-file-name)
      (call-process "yapf" nil nil nil "-i" buffer-file-name)))
  :hook ((python-mode . (lambda ()
				          (require 'lsp-pyright)
				          (add-hook 'after-save-hook #'lsp-pyright-format-buffer t t)))
	     ;; (pyvenv-mode . (lambda () (lsp-deferred)))
         )
  :init
  ;; too much noise in "real" projects
  (setq lsp-pyright-typechecking-mode "basic"
	    lsp-pyright-venv-path (file-truename "~/miniconda3/envs")))

;;; java

(use-package lsp-java
  :hook (java-mode . (lambda ()
		               (require 'lsp-java)
		               (lsp-deferred)))
  :init
  (setq lsp-java-import-maven-enabled t
	    lsp-java-implementations-code-lens-enabled t
	    lsp-java-save-actions-organize-imports t
	    ;; latest jdtls requires java >= 11 to work
	    lsp-java-java-path "/opt/jdk11/bin/java"
	    lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx6G" "-Xms100m")
	    ;; Runtime name must be one of: “J2SE-1.5”, “JavaSE-1.6”, “JavaSE-1.7”, “JavaSE-1.8” etc
	    ;; lsp-java-configuration-runtimes '[(:name "JavaSE-1.8"
	    ;; 				   :path "/opt/jdk/")
	    ;; 				  (:name "JavaSE-11"
	    ;; 				   :path "/opt/jdk11/"
	    ;; :default t)]
	    lsp-java-folding-range-enabled t))

;;; scala
(use-package lsp-metals)
(use-package consult-lsp)


(provide 'init-lsp)
