;; -*- lexical-binding: t no-byte-compile: t -*-

(when (equal petmacs-lsp-mode-impl 'eglot)
  (setq read-process-output-max (* 5 1024 1024)))

(use-package eglot
  :pin gnu
  :preface
  (defun petmacs/eglot-keybindgs ()
    (define-key evil-motion-state-map "gR" #'eglot-rename)
    (define-key evil-motion-state-map "gr" #'xref-find-references)
    (define-key evil-normal-state-map "gi" #'eglot-find-implementation)
    (define-key evil-motion-state-map "gh" #'eldoc-box-help-at-point)
    (define-key evil-normal-state-map "ga" #'eglot-code-actions))
  (defun petmacs/eglot-ensure-with-lsp-booster ()
    (when (fboundp 'eglot-booster-mode)
      (eglot-booster-mode t))
    (eglot-ensure))
  :hook (((c-mode c-ts-mode c++-mode c++-ts-mode) . petmacs/eglot-ensure-with-lsp-booster))
  :init
  (setq eglot-send-changes-idle-time 0.2
        eglot-autoshutdown t
        ;; eglot-connect-timeout 120
        eglot-connect-timeout 1200 ;; 10 minutes
        eglot-ignored-server-capabilities '(:inlayHintProvider)
        eldoc-echo-area-use-multiline-p nil
        ;; eglot-events-buffer-size 1
        eglot-server-programs '(
                                ((python-mode python-ts-mode) . ("pyright-langserver" "--stdio"))
                                ((c++-mode c-mode c++-ts-mode c-ts-mode objc-mode) . ("clangd"
                                                                                      ;; 在后台自动分析文件（基于complie_commands)
                                                                                      "--background-index"
                                                                                      ;; 标记compelie_commands.json文件的目录位置
                                                                                      "--compile-commands-dir=build"
                                                                                      ;; 全局补全（会自动补充头文件）
                                                                                      "--all-scopes-completion"
                                                                                      ;; 更详细的补全内容
                                                                                      "--completion-style=detailed"
                                                                                      ;; 同时开启的任务数量
                                                                                      "-j=12"
                                                                                      "-cross-file-rename"
                                                                                      ;;clang-tidy功能
                                                                                      "--clang-tidy"
                                                                                      "--clang-tidy-checks=performance-*,bugprone-*"
                                                                                      ;; 告诉clangd用那个clang进行编译，路径参考which clang++的路径
                                                                                      ;; "--query-driver=/opt/llvm/bin/clang++"
                                                                                      ;; 同时开启的任务数量
                                                                                      ;; 补充头文件的形式
                                                                                      ;; "--header-insertion=iwyu"
                                                                                      ;; pch优化的位置
                                                                                      ;; "--pch-storage=disk"
                                                                                      ))
                                ((cmake-mode cmake-ts-mode) . ("cmake-language-server"))
                                ((bash-ts-mode sh-mode) . ("bash-language-server" "start"))
                                ((go-mode go-dot-mod-mode go-dot-work-mode go-ts-mode go-mod-ts-mode) . ("gopls"))

                                ((java-mode java-ts-mode) . ("jdtls"))
                                ((yaml-ts-mode yaml-mode) . ("yaml-language-server" "--stdio"))
                                ((dockerfile-mode dockerfile-ts-mode) . ("docker-langserver" "--stdio"))))
  :config
  (push :documentHighlightProvider eglot-ignored-server-capabilities)
  (advice-add 'eglot-ensure :after 'petmacs/eglot-keybindgs))



(if petmacs-quelpa-use-gitee-mirror
    (use-package eglot-booster
      :quelpa (eglot-booster :fetcher git :url "https://gitee.com/Peter-Chou/eglot-booster.git" :upgrade t :files ("*.el")))
  (use-package eglot-booster
    :quelpa (eglot-booster :fetcher github :repo "jdtsmith/eglot-booster" :upgrade t :files ("*.el"))))

(use-package eglot-booster
  :after eglot
  :ensure nil
  :init (setq eglot-booster-no-remote-boost t)
  :config (eglot-booster-mode))

;; workaround
;; (defalias 'eglot-path-to-uri 'eglot--path-to-uri)
(use-package eglot-java
  :hook ((java-mode java-ts-mode) . (lambda ()
                                      (when (fboundp 'eglot-booster-mode)
                                        (eglot-booster-mode t))
                                      (eglot-java-mode t)))
  :init
  (setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name ".emacs.d/share/eclipse.jdt.ls/bin")))
  (setq exec-path (append exec-path (list (expand-file-name ".emacs.d/share/eclipse.jdt.ls/bin"))))

  (require 'eglot-java)
  (setq eglot-java-java-home "/opt/jdk17"
        eglot-java-java-program "/opt/jdk17/bin/java"
        eglot-java-eclipse-jdt-cache-directory (expand-file-name (file-name-concat user-emacs-directory "data" "eglot-java-cache"))
        eglot-java-eclipse-jdt-args
        `("-Xmx8G"
          ,(concat "-javaagent:" (expand-file-name "~/.emacs.d/data/lsp-java-jars/lombok-1.18.28.jar"))
          "--add-modules=ALL-SYSTEM"
          "--add-opens"
          "java.base/java.util=ALL-UNNAMED"
          "--add-opens"
          "java.base/java.lang=ALL-UNNAMED"
          "-XX:+UseZGC"
          "-XX:+UseStringDeduplication"
          ;; "-XX:FreqInlineSize=325"
          ;; "-XX:MaxInlineLevel=9"
          "-XX:+UseCompressedOops"))

  (defun petmacs/custom-eglot-java-init-opts
      (server eglot-java-eclipse-jdt)
    "Custom options that will be merged with default settings."
    `(:bundles [,(expand-file-name (file-name-concat
                                    user-emacs-directory "data"
                                    "lsp-java-jars"
                                    "com.microsoft.java.debug.plugin-0.51.1.jar"))]))
  (setq eglot-java-user-init-opts-fn 'petmacs/custom-eglot-java-init-opts)
  )

(use-package consult-eglot
  :init (setq consult-eglot-show-kind-name t))

(use-package consult-eglot-embark
  :demand t
  :after (embark consult-eglot)
  :config
  (consult-eglot-embark-mode))


(if petmacs-quelpa-use-gitee-mirror
    (use-package eglot-hierarchy
      :quelpa (eglot-hierarchy :fetcher git :url "https://gitee.com/Peter-Chou/eglot-hierarchy.git" :upgrade t :files ("*.el")))
  (use-package eglot-hierarchy
    :quelpa (eglot-hierarchy :fetcher github :repo "dolmens/eglot-hierarchy" :upgrade t :files ("*.el"))))

(use-package eglot-orderless
  :ensure nil
  :after (eglot orderless)
  :config
  (add-to-list 'completion-category-overrides
               '(eglot (orderless flex))))

(if petmacs-quelpa-use-gitee-mirror
    (use-package sideline-eglot
      :quelpa (sideline-eglot :fetcher git :url "https://gitee.com/Peter-Chou/sideline-eglot.git" :upgrade t :files ("*.el")))
  (use-package sideline-eglot
    :quelpa (sideline-eglot :fetcher github :repo "emacs-sideline/sideline-eglot" :upgrade t :files ("*.el"))))

(provide 'init-eglot)
