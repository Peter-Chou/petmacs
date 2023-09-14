;; -*- lexical-binding: t no-byte-compile: t -*-

(require 'init-custom)

;; optimize lsp-mode
(setenv "LSP_USE_PLISTS" "true")
(setq gc-cons-threshold 200000000 ;; 200mb
      read-process-output-max (* 3 1024 1024) ;; 3mb
      lsp-use-plists t
      lsp-log-io nil)

;; sudo apt-get install libjansson-dev
(use-package lsp-mode
  :preface
  (defun petmacs/lsp-find-definition-other-window ()
    (interactive)
    (switch-to-buffer-other-window (buffer-name))
    (lsp-find-definition))

  (defun petmacs/lsp-find-declaration-other-window ()
    (interactive)
    (switch-to-buffer-other-window (buffer-name))
    (lsp-find-declaration))

  (defun petmacs/lsp-find-implementation-other-window ()
    (interactive)
    (switch-to-buffer-other-window (buffer-name))
    (lsp-find-implementation))

  (defun petmacs/lsp-find-type-definition-other-window ()
    (interactive)
    (switch-to-buffer-other-window (buffer-name))
    (lsp-find-type-definition))

  (defun petmacs/lsp-find-references-other-window ()
    (interactive)
    (switch-to-buffer-other-window (buffer-name))
    (lsp-find-references))
  :functions nerd-icons-octicon
  :hook (((c-mode c-ts-mode c++-ts-mode c++-mode cuda-mode) . (lambda ()
					                                            (lsp-deferred)))
         ((scala-mode scala-ts-mode) . (lambda ()
			                             (require 'lsp-metals)
			                             (lsp-deferred)))
         ;; ((markdown-mode yaml-mode) . lsp-deferred)
         ((go-mode go-ts-mode) . lsp-deferred)
         ((javascript-mode javascript-ts-mode) . lsp-deferred)
         ((typescript-mode typescript-ts-mode) . lsp-deferred)
         (lsp-mode . (lambda ()
                       ;; Integrate `which-key'
                       (lsp-enable-which-key-integration)

                       ;; Format and organize imports
                       (unless (apply #'derived-mode-p petmacs-lsp-format-on-save-ignore-modes)
                         (add-hook 'before-save-hook #'lsp-format-buffer t t)
                         (add-hook 'before-save-hook #'lsp-organize-imports t t)))))
  :init
  (setq lsp-auto-guess-root nil
        ;; lsp-keymap-prefix "C-c l"
        ;; lsp-diagnostics-provider :none ;; flymake or ;; :none
        lsp-diagnostics-provider :flymake ;; flymake or ;; :none
        lsp-signature-auto-activate t
        lsp-signature-render-documentation nil
        lsp-modeline-code-actions-enable nil
        ;; lsp-modeline-diagnostics-enable t
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-diagnostics-scope :workspace
        lsp-modeline-workspace-status-enable nil
        lsp-headerline-breadcrumb-enable t
        ;; lsp-headerline-breadcrumb-segments '(path-up-to-project symbols)
        lsp-headerline-breadcrumb-segments '(symbols)

        ;; lsp-semantic-tokens-enable t
        lsp-semantic-tokens-enable nil  ;; do not color token by lsp
        lsp-progress-spinner-type 'progress-bar-filled

        ;; how often lsp-mode will refresh the highlights, lenses, links, etc while you type
        lsp-idle-delay 0.5

        lsp-keep-workspace-alive t
        lsp-enable-indentation nil
        lsp-enable-folding nil
        lsp-enable-file-watchers nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-text-document-color nil
        lsp-enable-on-type-formatting nil)
  :config
  (with-no-warnings
    ;; Disable `lsp-mode' in `git-timemachine-mode'
    (defun my-lsp--init-if-visible (fn &rest args)
      (unless (bound-and-true-p git-timemachine-mode)
        (apply fn args)))
    (advice-add #'lsp--init-if-visible :around #'my-lsp--init-if-visible)

    ;; Enable `lsp-mode' in sh/bash/zsh
    (defun my-lsp-bash-check-sh-shell (&rest _)
      (and (memq major-mode '(sh-mode bash-ts-mode))
           (memq sh-shell '(sh bash zsh))))
    (advice-add #'lsp-bash-check-sh-shell :override #'my-lsp-bash-check-sh-shell)
    (add-to-list 'lsp-language-id-configuration '(bash-ts-mode . "shellscript"))

    ;; Display icons
    (when (icons-displayable-p)
      (defun my-lsp-icons-get-symbol-kind (fn &rest args)

        (advice-add #'lsp-icons-get-by-symbol-kind :around #'my-lsp-icons-get-symbol-kind)

        ;; For `lsp-headerline'
        (defun my-lsp-icons-get-by-file-ext (fn &rest args)
          (and (icons-displayable-p) (apply fn args)))
        (advice-add #'lsp-icons-get-by-file-ext :around #'my-lsp-icons-get-by-file-ext)

        (defun my-lsp-icons-get-by-file-ext (file-ext &optional feature)
          (when (and file-ext
                     (lsp-icons--enabled-for-feature feature))
            (nerd-icons-icon-for-extension file-ext)))
        (advice-add #'lsp-icons-get-by-file-ext :override #'my-lsp-icons-get-by-file-ext)

        (defvar lsp-symbol-alist
          '((misc          nerd-icons-codicon "nf-cod-symbol_namespace" :face font-lock-warning-face)
            (document      nerd-icons-codicon "nf-cod-symbol_file" :face font-lock-string-face)
            (namespace     nerd-icons-codicon "nf-cod-symbol_namespace" :face font-lock-type-face)
            (string        nerd-icons-codicon "nf-cod-symbol_string" :face font-lock-doc-face)
            (boolean-data  nerd-icons-codicon "nf-cod-symbol_boolean" :face font-lock-builtin-face)
            (numeric       nerd-icons-codicon "nf-cod-symbol_numeric" :face font-lock-builtin-face)
            (method        nerd-icons-codicon "nf-cod-symbol_method" :face font-lock-function-name-face)
            (field         nerd-icons-codicon "nf-cod-symbol_field" :face font-lock-variable-name-face)
            (localvariable nerd-icons-codicon "nf-cod-symbol_variable" :face font-lock-variable-name-face)
            (class         nerd-icons-codicon "nf-cod-symbol_class" :face font-lock-type-face)
            (interface     nerd-icons-codicon "nf-cod-symbol_interface" :face font-lock-type-face)
            (property      nerd-icons-codicon "nf-cod-symbol_property" :face font-lock-variable-name-face)
            (indexer       nerd-icons-codicon "nf-cod-symbol_enum" :face font-lock-builtin-face)
            (enumerator    nerd-icons-codicon "nf-cod-symbol_enum" :face font-lock-builtin-face)
            (enumitem      nerd-icons-codicon "nf-cod-symbol_enum_member" :face font-lock-builtin-face)
            (constant      nerd-icons-codicon "nf-cod-symbol_constant" :face font-lock-constant-face)
            (structure     nerd-icons-codicon "nf-cod-symbol_structure" :face font-lock-variable-name-face)
            (event         nerd-icons-codicon "nf-cod-symbol_event" :face font-lock-warning-face)
            (operator      nerd-icons-codicon "nf-cod-symbol_operator" :face font-lock-comment-delimiter-face)
            (template      nerd-icons-codicon "nf-cod-symbol_snippet" :face font-lock-type-face)))

        (defun my-lsp-icons-get-by-symbol-kind (kind &optional feature)
          (when (and kind
                     (lsp-icons--enabled-for-feature feature))
            (let* ((icon (cdr (assoc (lsp-treemacs-symbol-kind->icon kind) lsp-symbol-alist)))
                   (args (cdr icon)))
              (apply (car icon) args))))
        (advice-add #'lsp-icons-get-by-symbol-kind :override #'my-lsp-icons-get-by-symbol-kind)

        (setq lsp-headerline-arrow (nerd-icons-octicon "nf-oct-chevron_right"
                                                       :face 'lsp-headerline-breadcrumb-separator-face))))
    (with-eval-after-load 'pyvenv
      (add-hook 'pyvenv-post-activate-hooks #'lsp-deferred))))


(use-package lsp-ui
  :custom-face
  (lsp-ui-sideline-code-action ((t (:inherit warning))))
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init (setq lsp-ui-sideline-show-diagnostics nil
	          lsp-ui-sideline-enable nil
	          lsp-ui-sideline-show-code-actions nil
              lsp-ui-sideline-ignore-duplicate t
	          lsp-ui-doc-enable nil
	          lsp-ui-doc-delay 0.1
              lsp-ui-imenu-auto-refresh 'after-save
	          lsp-ui-doc-border (face-foreground 'font-lock-comment-face nil t)
              lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
				                    ,(face-foreground 'font-lock-string-face)
				                    ,(face-foreground 'font-lock-constant-face)
				                    ,(face-foreground 'font-lock-variable-name-face)))
  ;; Set correct color to borders
  (defun my-lsp-ui-doc-set-border ()
    "Set the border color of lsp doc."
    (setq lsp-ui-doc-border
          (if (facep 'posframe-border)
              (face-background 'posframe-border nil t)
            (face-background 'region nil t))))
  (my-lsp-ui-doc-set-border)
  (add-hook 'after-load-theme-hook #'my-lsp-ui-doc-set-border t))

(use-package lsp-treemacs
  :after lsp-mode
  :functions nerd-icons-codicon
  :init
  ;; (setq
  ;;  lsp-treemacs-deps-position-params
  ;;  `((side . right)
  ;;    (slot . 1)
  ;;    (window-width . 32))
  ;;  lsp-treemacs-symbols-position-params
  ;;  `((side . right)
  ;;    (slot . 2)
  ;;    (window-width . 32)))
  (lsp-treemacs-sync-mode 1)
  :config
  (define-key winum-keymap (kbd "C-M-9") 'lsp-treemacs-symbols)
     (with-no-warnings
       (when (icons-displayable-p)
         (treemacs-create-theme "lsp-nerd-icons"
           :config
           (progn
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-repo" :face 'nerd-icons-blue))
              :extensions (root))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_boolean" :face 'nerd-icons-lblue))
              :extensions (boolean-data))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_class" :face 'nerd-icons-orange))
              :extensions (class))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_color"))
              :extensions (color-palette))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_constant"))
              :extensions (constant))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_file"))
              :extensions (document))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_misc" :face 'nerd-icons-orange))
              :extensions (enumerator))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_enum_member" :face 'nerd-icons-lblue))
              :extensions (enumitem))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_event" :face 'nerd-icons-orange))
              :extensions (event))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_field" :face 'nerd-icons-lblue))
              :extensions (field))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_misc"))
              :extensions (indexer))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_keyword"))
              :extensions (intellisense-keyword))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_interface" :face 'nerd-icons-lblue))
              :extensions (interface))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_variable" :face 'nerd-icons-lblue))
              :extensions (localvariable))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_method" :face 'nerd-icons-purple))
              :extensions (method))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_namespace" :face 'nerd-icons-lblue))
              :extensions (namespace))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_numeric"))
              :extensions (numeric))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_operator"))
              :extensions (operator))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_property"))
              :extensions (property))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_snippet"))
              :extensions (snippet))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_string"))
              :extensions (string))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_structure" :face 'nerd-icons-orange))
              :extensions (structure))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_snippet"))
              :extensions (template))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-chevron_right" :face 'nerd-icons-dsilver))
              :extensions (collapsed) :fallback "+")
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-chevron_down" :face 'nerd-icons-dsilver))
              :extensions (expanded) :fallback "-")
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-file_binary" :face 'nerd-icons-dsilver))
              :extensions (classfile))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-folder_opened" :face 'nerd-icons-blue))
              :extensions (default-folder-opened))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-folder" :face 'nerd-icons-blue))
              :extensions (default-folder))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-folder_opened" :face 'nerd-icons-green))
              :extensions (default-root-folder-opened))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-folder" :face 'nerd-icons-green))
              :extensions (default-root-folder))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-file_binary" :face 'nerd-icons-dsilver))
              :extensions ("class"))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-file_zip" :face 'nerd-icons-dsilver))
              :extensions (file-type-jar))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-folder_opened" :face 'nerd-icons-dsilver))
              :extensions (folder-open))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-folder" :face 'nerd-icons-dsilver))
              :extensions (folder))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-folder_opened" :face 'nerd-icons-orange))
              :extensions (folder-type-component-opened))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-folder" :face 'nerd-icons-orange))
              :extensions (folder-type-component))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-folder_opened" :face 'nerd-icons-green))
              :extensions (folder-type-library-opened))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-folder" :face 'nerd-icons-green))
              :extensions (folder-type-library))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-folder_opened" :face 'nerd-icons-pink))
              :extensions (folder-type-maven-opened))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-folder" :face 'nerd-icons-pink))
              :extensions (folder-type-maven))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-folder_opened" :face 'nerd-icons-orange))
              :extensions (folder-type-package-opened))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-folder" :face 'nerd-icons-orange))
              :extensions (folder-type-package))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-add" :face 'nerd-icons-dsilver))
              :extensions (icon-create))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-list_flat" :face 'nerd-icons-dsilver))
              :extensions (icon-flat))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_class" :face 'nerd-icons-blue))
              :extensions (icon-hierarchical))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-link" :face 'nerd-icons-dsilver))
              :extensions (icon-link))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-refresh" :face 'nerd-icons-dsilver))
              :extensions (icon-refresh))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-faicon "nf-fa-unlink" :face 'nerd-icons-dsilver))
              :extensions (icon-unlink))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-devicon "nf-dev-java" :face 'nerd-icons-orange))
              :extensions (jar))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-library" :face 'nerd-icons-green))
              :extensions (library))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-folder_opened" :face 'nerd-icons-lblue))
              :extensions (packagefolder-open))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-folder" :face 'nerd-icons-lblue))
              :extensions (packagefolder))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-archive" :face 'nerd-icons-dsilver))
              :extensions (package))
             (treemacs-create-icon
              :icon (format "%s " (nerd-icons-codicon "nf-cod-repo" :face 'nerd-icons-blue))
              :extensions (java-project))))

         (setq lsp-treemacs-theme "lsp-nerd-icons"))))

;;; python
(use-package lsp-pyright
  :preface
  ;; Use yapf to format
  (defun lsp-pyright-format-buffer ()
	(interactive)
	(when (and (executable-find "yapf") buffer-file-name)
      (call-process "yapf" nil nil nil "-i" buffer-file-name)))
  :hook (((python-mode python-ts-mode) . (lambda ()
				                           (require 'lsp-pyright)
				                           (add-hook 'after-save-hook #'lsp-pyright-format-buffer t t))))
  :init
  ;; too much noise in "real" projects
  (setq ;; lsp-pyright-typechecking-mode "basic"
   lsp-pyright-venv-path (getenv "WORKON_HOME")))

;;; java
(use-package lsp-java
  :hook ((java-mode java-ts-mode jdee-mode) . (lambda ()
		                                        (require 'lsp-java)
                                                (require 'lsp-java-boot)
		                                        (lsp-deferred)))
  :init
  (setq
   ;; Use Google style formatting by default
   lsp-java-format-settings-url
   "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
   lsp-java-format-settings-profile "GoogleStyle"

   lsp-java-import-maven-enabled t
   lsp-java-implementations-code-lens-enabled t
   lsp-java-save-actions-organize-imports t
   lsp-java-vmargs '("-XX:+UseParallelGC"
                     "-XX:GCTimeRatio=4"
                     "-XX:AdaptiveSizePolicyWeight=90"
                     "-Dsun.zip.disableMemoryMapping=true"
                     "-Xmx6G"
                     "-Xms100m"
                     "-javaagent:/home/peter/.m2/repository/org/projectlombok/lombok/1.18.28/lombok-1.18.28.jar")
   lsp-java-folding-range-enabled t)

  ;; (setenv "JAVA_HOME" "/opt/jdk17")
  ;; latest jdtls requires java >= 17 to work
  (setq lsp-java-java-path "/opt/jdk17/bin/java")

  ;; use jdtls 1.23 released at 2023.4.27
  ;; (setq lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.25.0/jdt-language-server-1.25.0-202306291518.tar.gz")

  (add-hook 'conf-javaprop-mode-hook #'lsp))

;; (use-package lsp-java-lombok
;;   :load-path (lambda () (expand-file-name "site-lisp/lsp-java-lombok" user-emacs-directory))
;;   :after lsp-java
;;   :init
;;   (setq lsp-java-lombok/enabled t
;;         lsp-java-lombok/version "1.18.24")
;;   (require 'lsp-java-lombok)
;;   (lsp-java-lombok/init))

;;; scala
(use-package lsp-metals)
(use-package consult-lsp)

(use-package lsp-focus
  :after focus
  :hook (focus-mode . lsp-focus-mode))

(provide 'init-lsp)
