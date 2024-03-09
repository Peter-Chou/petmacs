;; -*- lexical-binding: t no-byte-compile: t -*-

(setq read-process-output-max (* 5 1024 1024)) ;; 5mb

(with-no-warnings
  (use-package eglot
    :preface
    (defun petmacs/eglot-keybindgs ()
      (define-key evil-motion-state-map "gR" #'eglot-rename)
      (define-key evil-motion-state-map "gr" #'xref-find-references)
      (define-key evil-normal-state-map "gi" #'eglot-find-implementation)
      (define-key evil-motion-state-map "gh" #'eldoc)
      (define-key evil-normal-state-map "ga" #'eglot-code-actions))
    :init
    (setq eglot-send-changes-idle-time 0.2
          eglot-autoshutdown t
          eglot-connect-timeout 120
          eglot-ignored-server-capabilities '(:inlayHintProvider)
          eldoc-echo-area-use-multiline-p nil
          ;; eglot-events-buffer-size 1
          eglot-server-programs '(
                                  ((python-mode python-ts-mode) . ("pyright-langserver" "--stdio"))
                                  ((c++-mode c-mode c++-ts-mode c-ts-mode objc-mode) . ("clangd"))
                                  ((cmake-mode cmake-ts-mode) . ("cmake-language-server"))
                                  ((bash-ts-mode sh-mode) . ("bash-language-server" "start"))
                                  ((go-mode go-dot-mod-mode go-dot-work-mode go-ts-mode go-mod-ts-mode) . ("gopls"))

                                  ;; ((java-mode java-ts-mode) . ("jdtls"))
                                  ((yaml-ts-mode yaml-mode) . ("yaml-language-server" "--stdio"))
                                  ((dockerfile-mode dockerfile-ts-mode) . ("docker-langserver" "--stdio")))
          )

    :config
    (push :documentHighlightProvider eglot-ignored-server-capabilities)

    ;; jdtls download see https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones

    ;; (defun petmacs/jdtls-command-contact (interactive)
    ;;   (let* ((jdtls-cache-dir (file-name-concat user-emacs-directory "data" "eglot-cache"))
    ;;          (project-dir (file-name-nondirectory (directory-file-name (project-root p))))
    ;;          (data-dir (expand-file-name (file-name-concat jdtls-cache-dir project-dir)))
    ;;          (jvm-args `(,(concat "-javaagent:" (expand-file-name "data/lombok-1.18.28.jar" user-emacs-directory))
    ;;                      "-Xmx8G"
    ;;                      ;; "-XX:+UseG1GC"
    ;;                      "-XX:+UseZGC"
    ;;                      "-XX:+UseStringDeduplication"
    ;;                      ;; "-XX:FreqInlineSize=325"
    ;;                      ;; "-XX:MaxInlineLevel=9"
    ;;                      "-XX:+UseCompressedOops"))
    ;;          (jvm-args (mapcar (lambda (arg) (concat "--jvm-arg=" arg)) jvm-args))
    ;;          ;; tell jdtls the data directory and jvm args
    ;;          (contact (append '("jdtls") jvm-args `("-data" ,data-dir))))
    ;;     contact))
    (require 'eglot)

    ;; (defun jdtls-command-contact (&optional interactive)
    ;;   (let* ((jdtls-cache-dir (file-name-concat user-emacs-directory "cache" "lsp-cache"))
    ;;          (project-dir (file-name-nondirectory (directory-file-name (project-root (project-current)))))
    ;;          (data-dir (expand-file-name (file-name-concat jdtls-cache-dir (md5 project-dir))))
    ;;          (jvm-args `(,(concat "-javaagent:" (expand-file-name "~/.emacs.d/data/lombok-1.18.28.jar"))
    ;;                      "-Xmx8G"
    ;;                      ;; "-XX:+UseG1GC"
    ;;                      "-XX:+UseZGC"
    ;;                      "-XX:+UseStringDeduplication"
    ;;                      ;; "-XX:FreqInlineSize=325"
    ;;                      ;; "-XX:MaxInlineLevel=9"
    ;;                      "-XX:+UseCompressedOops"))
    ;;          (jvm-args (mapcar (lambda (arg) (concat "--jvm-arg=" arg)) jvm-args))
    ;;          ;; tell jdtls the data directory and jvm args
    ;;          (contact (append '("jdtls") jvm-args `("-data" ,data-dir))))
    ;;     contact))

    ;; (push '((java-mode java-ts-mode) . (jdtls-command-contact)) eglot-server-programs)

    (add-to-list 'eglot-server-programs
	             '(java-ts-mode . (lambda (&optional interactive)
                                    ("jdtls"))))




    ;; (defvar +eglot/initialization-options-map (make-hash-table :size 5))

    ;; (cl-defmethod eglot-initialization-options ((server eglot-lsp-server))
    ;;   (if-let ((init-options (gethash (eglot--major-mode server) +eglot/initialization-options-map)))
    ;;       init-options
    ;;     eglot--{}))


    ;; (add-to-list 'eglot-server-programs
    ;;              `((java-mode java-ts-mode) ("jdtls"
    ;;                                          "-configuration" ,(expand-file-name "cache/language-server/java/jdtls/config_linux" user-emacs-directory)
    ;;                                          "-data" ,(expand-file-name "cache/java-workspace" user-emacs-directory)
    ;;                                          ,(concat "--jvm-arg=-javaagent:" (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.20/lombok-1.18.20.jar")))))

    ;; (puthash 'java-ts-mode
    ;;          `(:settings
    ;;            (:java
    ;;             (:configuration
    ;;              (:runtime [(:name "JavaSE-1.8" :path "/opt/jdk8")
    ;;                         (:name "JavaSE-11" :path "/opt/jdk11")
    ;;                         (:name "JavaSE-17" :path "/opt/jdk17" :default t)])
    ;;              :format (:settings (:url ,(expand-file-name "eclipse-java-google-style.xml"
    ;;                                                          (concat user-emacs-directory "data"))
    ;;                                  :profile "GoogleStyle"))
    ;;              ;; NOTE: https://github.com/redhat-developer/vscode-java/issues/406#issuecomment-356303715
    ;;              ;; > We enabled it by default so that workspace-wide errors can be reported (eg. removing a public method in one class would cause compilation errors in other files consuming that method).
    ;;              ;; for large workspaces, it may make sense to be able to disable autobuild if it negatively impacts performance.
    ;;              :autobuild (:enabled t)
    ;;              ;; https://github.com/dgileadi/vscode-java-decompiler
    ;;              :contentProvider (:preferred "fernflower")))
    ;;            ;; WIP: support non standard LSP `java/classFileContents', `Location' items that have a `jdt://...' uri
    ;;            ;; https://github.com/eclipse/eclipse.jdt.ls/issues/1384
    ;;            ;; nvim impl demo: https://github.com/mfussenegger/dotfiles/commit/3cddf73cd43120da2655e2df6d79bdfd06697f0e
    ;;            ;; lsp-java impl demo: https://github.com/emacs-lsp/lsp-java/blob/master/lsp-java.el
    ;;            :extendedClientCapabilities (:classFileContentsSupport t)
    ;;            ;; bundles: decompilers, etc.
    ;;            ;; https://github.com/dgileadi/dg.jdt.ls.decompiler
    ;;            ;; :bundles ,(let ((bundles-dir (expand-file-name (locate-user-emacs-file "cache/language-server/java/bundles" user-emacs-directory)))
    ;;            ;;                 jdtls-bundles)
    ;;            ;;             (->> (when (file-directory-p bundles-dir)
    ;;            ;;                    (directory-files bundles-dir t "\\.jar$"))
    ;;            ;;                  (append jdtls-bundles)
    ;;            ;;                  (apply #'vector)))
    ;;            )
    ;;          +eglot/initialization-options-map
    ;;          )

    (advice-add 'eglot-ensure :after 'petmacs/eglot-keybindgs)
    :config
    ))

(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name ".emacs.d/share/eclipse.jdt.ls/bin")))
(setq exec-path (append exec-path (list (expand-file-name ".emacs.d/share/eclipse.jdt.ls/bin"))))

(use-package eglot-java
  :init
  (require 'eglot-java)
  (setq eglot-java-java-home "/opt/jdk17"
        ;; eglot-java-eglot-server-programs-manual-updates t
        ;; eglot-java-eclipse-jdt-args
        ;; `("-Xmx8G"
        ;;   "-XX:+UseZGC"
        ;;   "-XX:+UseStringDeduplication"
        ;;   "-XX:+UseCompressedOops"
        ;;   "--add-modules=ALL-SYSTEM"
        ;;   ,(concat "-javaagent:" (expand-file-name "data/lombok-1.18.28.jar" user-emacs-directory))
        ;;   "--add-opens"
        ;;   "java.base/java.util=ALL-UNNAMED"
        ;;   "--add-opens"
        ;;   "java.base/java.lang=ALL-UNNAMED")
        )

  ;; (setq eglot-java-user-init-opts-fn 'custom-eglot-java-init-opts)
  ;; (defun custom-eglot-java-init-opts (server eglot-java-eclipse-jdt)
  ;;   "Custom options that will be merged with any default settings."
  ;;   `(:settings
  ;;     (:java
  ;;      (:format (:settings (:url ,(expand-file-name "eclipse-java-google-style.xml" (concat user-emacs-directory "data"))
  ;;                           :profile "GoogleStyle")
  ;;                ;; :enabled t
  ;;                ))
  ;;      (:import (:maven (:enabled t)))
  ;;      (:exclusions '("**/node_modules/**" "**/.metadata/**" "**/archetype-resources/**" "**/META-INF/maven/**"))
  ;;      )))
  :config
  ;; (add-hook 'java-mode-hook 'eglot-java-mode)
  ;; (add-hook 'java-ts-mode-hook 'eglot-java-mode)
  (require 'eglot-java)
  )

(use-package consult-eglot)

(use-package eglot-orderless
  :ensure nil
  :after (eglot orderless)
  :config
  (add-to-list 'completion-category-overrides
               '(eglot (orderless flex))))

(use-package eglot-booster
  :quelpa (eglot-booster :fetcher github :repo "jdtsmith/eglot-booster" :files ("*.el"))
  ;; :quelpa (eglot-booster :fetcher git :url "https://gitee.com/Peter-Chou/eglot-booster.git" :files ("*.el"))
  :config	(eglot-booster-mode))

(provide 'init-eglot)
