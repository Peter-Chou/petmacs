;; -*- lexical-binding: t no-byte-compile: t -*-

(when (equal petmacs-lsp-mode-impl 'eglot)
  (setq read-process-output-max (* 5 1024 1024)))

(with-no-warnings
  (use-package eglot
    :preface
    (defun petmacs/eglot-keybindgs ()
      (define-key evil-motion-state-map "gR" #'eglot-rename)
      (define-key evil-motion-state-map "gr" #'xref-find-references)
      (define-key evil-normal-state-map "gi" #'eglot-find-implementation)
      ;; (define-key evil-motion-state-map "gh" #'eldoc)
      (define-key evil-motion-state-map "gh" #'eldoc-box-help-at-point)
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

                                  ((java-mode java-ts-mode) . ("jdtls"))
                                  ((yaml-ts-mode yaml-mode) . ("yaml-language-server" "--stdio"))
                                  ((dockerfile-mode dockerfile-ts-mode) . ("docker-langserver" "--stdio"))))
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

    ;; (defun jdtls-command-contact (&optional interactive)
    ;;   (let* ((jvm-args `(,(concat "-javaagent:" (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.20/lombok-1.18.20.jar"))
    ;;                      "-Xmx8G"
    ;;                      ;; "-XX:+UseG1GC"
    ;;                      "-XX:+UseZGC"
    ;;                      "-XX:+UseStringDeduplication"
    ;;                      ;; "-XX:FreqInlineSize=325"
    ;;                      ;; "-XX:MaxInlineLevel=9"
    ;;                      "-XX:+UseCompressedOops"))
    ;;          (jvm-args (mapcar (lambda (arg) (concat "--jvm-arg=" arg)) jvm-args))
    ;;          ;; tell jdtls the data directory and jvm args
    ;;          (contact (append '("jdtls") jvm-args)))
    ;;     contact))

    ;; (push '((java-mode java-ts-mode) . jdtls-command-contact) eglot-server-programs)

    (advice-add 'eglot-ensure :after 'petmacs/eglot-keybindgs)
    ))

(use-package eglot-booster
  :after eglot
  :quelpa (eglot-booster :fetcher github :repo "jdtsmith/eglot-booster" :files ("*.el"))
  ;; :quelpa (eglot-booster :fetcher git :url "https://gitee.com/Peter-Chou/eglot-booster.git" :files ("*.el"))
  ;; :init (setq eglot-booster-no-remote-boost t)
  :config (eglot-booster-mode))

(use-package eglot-java
  :hook ((java-mode java-ts-mode) . (lambda ()
                                      (eglot-booster-mode t)
                                      (eglot-java-mode t)))
  :init
  (setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name ".emacs.d/share/eclipse.jdt.ls/bin")))
  (setq exec-path (append exec-path (list (expand-file-name ".emacs.d/share/eclipse.jdt.ls/bin"))))

  ;; workaround
  (defalias 'eglot-path-to-uri 'eglot--path-to-uri)
  (require 'eglot-java)
  (setq eglot-java-java-home "/opt/jdk17"
        eglot-java-eclipse-jdt-cache-directory (expand-file-name (file-name-concat user-emacs-directory "data" "eglot-java-cache"))
        eglot-java-eclipse-jdt-args
        `("-Xmx8G"
          ,(concat "-javaagent:" (expand-file-name "~/.emacs.d/data/lombok-1.18.28.jar"))
          "--add-modules=ALL-SYSTEM"
          "--add-opens"
          "java.base/java.util=ALL-UNNAMED"
          "--add-opens"
          "java.base/java.lang=ALL-UNNAMED"
          "-XX:+UseZGC"
          "-XX:+UseStringDeduplication"
          ;; "-XX:FreqInlineSize=325"
          ;; "-XX:MaxInlineLevel=9"
          "-XX:+UseCompressedOops")))

(use-package consult-eglot
  :init (setq consult-eglot-show-kind-name t))

(use-package eglot-hierarchy
  :quelpa (eglot-hierarchy :fetcher github :repo "dolmens/eglot-hierarchy" :files ("*.el"))
  ;; :quelpa (eglot-booster :fetcher git :url "https://gitee.com/Peter-Chou/eglot-hierarchy.git" :files ("*.el"))
  )

(use-package eglot-orderless
  :ensure nil
  :after (eglot orderless)
  :config
  (add-to-list 'completion-category-overrides
               '(eglot (orderless flex))))

(provide 'init-eglot)
