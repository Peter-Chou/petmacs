;; -*- lexical-binding: t no-byte-compile: t -*-

(when (equal petmacs-lsp-mode-impl 'eglot)
  (setq read-process-output-max (* 5 1024 1024)))

(use-package eldoc-box)

(use-package eglot
  :pin gnu
  ;; :pin gnu-devel
  :preface
  (defun petmacs/eglot-keybindgs ()
    (define-key evil-motion-state-map "gR" #'eglot-rename)
    (define-key evil-motion-state-map "gr" #'xref-find-references)
    (define-key evil-normal-state-map "gi" #'eglot-find-implementation)
    (define-key evil-motion-state-map "gh" #'eldoc-box-help-at-point)
    (define-key evil-normal-state-map "ga" #'eglot-code-actions))

  (defun petmacs/eglot-ensure-with-lsp-booster (&optional exe)
    (let ((enable-lsp (if (and exe (stringp exe))
                          (stringp (executable-find exe))
                        t))
          (enable-booster (and emacs/>=29p
                               petmacs-use-lsp-booster
                               (fboundp 'eglot-booster-mode)
                               (executable-find "emacs-lsp-booster"))))
      (when enable-lsp
        (when enable-booster
          (eglot-booster-mode t))
        (eglot-ensure))))

  (defun petmacs/basedpyright-eglot-workspace-config (server)
    '(:basedpyright\.analysis (
                               ;; :pythonVersion "3.11"
                               :typeCheckingMode "off"
                               :diagnosticMode "workspace"
                               :pythonPlatform: "Linux"
                               :autoSearchPaths t
                               :extraPaths ["src"]
                               :logLevel "Warning"
                               :exclude ["data" "ckpts" "notebooks"
                                         "resources" "model_repository"
                                         "model_repositories" "typings"]
                               :useLibraryCodeForTypes t)))

  (defun petmacs/delance-eglot-workspace-config (server)
    '(:python.languageServer "Pylance"
      :python.analysis (
                        ;; :pythonVersion "3.11"
                        :typeCheckingMode "off"
                        :languageServerMode "full"
                        :diagnosticMode "workspace"
                        :pythonPlatform: "Linux"
                        :autoSearchPaths t
                        :extraPaths ["src"]
                        :logLevel "Warning"
                        :exclude ["data" "ckpts" "notebooks"
                                  "resources" "model_repository"
                                  "model_repositories" "typings"]
                        :useLibraryCodeForTypes t)))
  :hook (((c-mode c-ts-mode c++-mode c++-ts-mode) . (lambda ()
                                                      (petmacs/eglot-ensure-with-lsp-booster "clangd")))
         ((bash-ts-mode sh-mode) . (lambda ()
                                     (petmacs/eglot-ensure-with-lsp-booster "bash-language-server")))
         ((cmake-mode cmake-ts-mode) . (lambda ()
                                         (petmacs/eglot-ensure-with-lsp-booster "neocmakelsp")))
         ((dockerfile-mode dockerfile-ts-mode) . (lambda ()
                                                   (petmacs/eglot-ensure-with-lsp-booster "docker-langserver"))))
  :init
  (setq eglot-send-changes-idle-time 0.2
        eglot-autoshutdown t
        eglot-connect-timeout 1200 ;; 10 minutes
        eglot-ignored-server-capabilities '(:documentHighlightProvider
                                            :inlayHintProvider
                                            :documentOnTypeFormattingProvider)
        eldoc-echo-area-use-multiline-p nil
        eglot-server-programs
        '(((c++-mode c-mode c++-ts-mode c-ts-mode objc-mode) . ("clangd"
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

          ;; ((cmake-mode cmake-ts-mode) . ("cmake-language-server"))
          ((cmake-mode cmake-ts-mode) . ("neocmakelsp" "--stdio"))

          ((python-mode python-ts-mode) . ("basedpyright-langserver" "--watch" "--threads 12" "--stdio"))
          ;; ((python-mode python-ts-mode) . ("delance-langserver" "--stdio"))
          ;; ((python-mode python-ts-mode) . ("ty" "server"))

          ((java-mode java-ts-mode) . ("jdtls"))

          ((go-mode go-dot-mod-mode go-dot-work-mode go-ts-mode go-mod-ts-mode go-work-ts-mode) . ("gopls"))

          ((bash-ts-mode sh-mode) . ("bash-language-server" "start"))
          ((yaml-ts-mode yaml-mode) . ("yaml-language-server" "--stdio"))
          ((markdown-ts-mode markdown-mode) . ("marksman" "server"))
          ((dockerfile-mode dockerfile-ts-mode) . ("docker-langserver" "--stdio"))))
  :config
  (setq-default eglot-workspace-configuration #'petmacs/basedpyright-eglot-workspace-config)
  ;; (setq-default eglot-workspace-configuration #'petmacs/delance-eglot-workspace-config)

  (advice-add 'eglot-ensure :after 'petmacs/eglot-keybindgs))

;; (use-package eglot-booster
;;   :after eglot
;;   :ensure nil
;;   :demand t
;;   :init (setq eglot-booster-no-remote-boost t))

(use-package eglot-java
  :preface
  (defun petmacs/eglot-java-run-test-in-debug ()
    (interactive)
    (eglot-java-run-test t))
  (defun petmacs/eglot-java-run-main-in-debug ()
    (interactive)
    (eglot-java-run-main t))
  :hook ((java-mode java-ts-mode) . (lambda ()
                                      (when (and emacs/>=29p
                                                 petmacs-use-lsp-booster
                                                 (fboundp 'eglot-booster-mode)
                                                 (executable-find "emacs-lsp-booster"))
                                        (eglot-booster-mode t))
                                      (eglot-java-mode t)))
  :init
  (setq eglot-java-eclipse-jdt-args '("-Xmx6G" ;; 最大堆内存
                                      "-Xms6G" ;; # 初始堆内存（与 -Xmx 一致，避免动态扩容开销）
                                      "--add-modules=ALL-SYSTEM"
                                      "--add-opens"
                                      "java.base/java.util=ALL-UNNAMED"
                                      "--add-opens"
                                      "java.base/java.lang=ALL-UNNAMED"))
  (setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name ".emacs.d/share/eclipse.jdt.ls/bin")))
  (setq exec-path (append exec-path (list (expand-file-name ".emacs.d/share/eclipse.jdt.ls/bin"))))

  (require 'eglot-java)
  (setq eglot-java-java-home "/opt/jdk21"
        eglot-java-java-program "/opt/jdk21/bin/java"
        eglot-java-eclipse-jdt-cache-directory (expand-file-name (file-name-concat user-emacs-directory "data" "eglot-java-cache"))
        eglot-java-debug-jvm-arg "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=localhost:11111"
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
                                    "com.microsoft.java.debug.plugin-0.52.0.jar"))]
      :extendedClientCapabilities (:classFileContentsSupport t)))
  (setq eglot-java-user-init-opts-fn 'petmacs/custom-eglot-java-init-opts)
  :config
  (with-eval-after-load 'dape
    (add-to-list 'dape-configs
                 `(jdtls
                   modes (java-mode java-ts-mode)
                   fn (lambda (config)
                        (with-current-buffer
                            (find-file-noselect (expand-file-name (plist-get config :program)
                                                                  (project-root (project-current))))
                          (thread-first
                            config
                            (plist-put 'hostname "localhost")
                            (plist-put 'port (eglot-execute-command (eglot-current-server)
                                                                    "vscode.java.startDebugSession" nil))
                            (plist-put :projectName (project-name (project-current))))))
                   :program dape-buffer-default
                   :request "attach"
                   :hostname "localhost"
                   :port 11111))))

(use-package consult-eglot
  :init (setq consult-eglot-show-kind-name t))

(use-package consult-eglot-embark
  :demand t
  :after (embark consult-eglot)
  :config
  (consult-eglot-embark-mode))

(use-package eglot-orderless
  :ensure nil
  :after (eglot orderless)
  :config
  (add-to-list 'completion-category-overrides
               '(eglot (orderless flex))))

(use-package sideline-eglot
  :pin melpa
  :custom-face
  (sideline-eglot-error ((t (:height 0.85 :italic t))))
  (sideline-eglot-warning ((t (:height 0.85 :italic t))))
  (sideline-eglot-success ((t (:height 0.85 :italic t)))))

(cl-defmacro eglot-org-babel-enable (lang)
  "Support LANG in org source code block."
  (cl-check-type lang string)
  (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
         (intern-pre (intern (format "eglot--%s" (symbol-name edit-pre)))))
    `(progn
       (defun ,intern-pre (info)
         (let ((file-name (->> info caddr (alist-get :file))))
           (unless file-name
             (setq file-name (concat default-directory (if (string= ,lang "C") "org-src-babel.c" "org-src-babel.cpp")))
             (write-region (point-min) (point-max) file-name))
           (setq buffer-file-name file-name)
           (eglot-ensure)))
       (put ',intern-pre 'function-documentation
            (format "Enable eglot mode in the buffer of org source block (%s)."
                    (upcase ,lang)))
       (if (fboundp ',edit-pre)
           (advice-add ',edit-pre :after ',intern-pre)
         (progn
           (defun ,edit-pre (info)
             (,intern-pre info))
           (put ',edit-pre 'function-documentation
                (format "Prepare local buffer environment for org source block (%s)."
                        (upcase ,lang))))))))

(with-eval-after-load 'org
  (dolist (lang '("C" "C++"))
    (eval `(eglot-org-babel-enable ,lang))))

(provide 'init-eglot)
