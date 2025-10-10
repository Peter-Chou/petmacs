;; -*- lexical-binding: t no-byte-compile: t -*-

(eval-when-compile
  (require 'init-custom)
  (require 'init-funcs))

;; optimize lsp-mode
(when (equal petmacs-lsp-mode-impl 'lsp-mode)
  (setenv "LSP_USE_PLISTS" "true")
  (setq read-process-output-max (* 5 1024 1024) ;; 5mb
        lsp-use-plists t
        lsp-log-io nil))

;; sudo apt-get install libjansson-dev
(use-package lsp-mode
  :demand t
  :functions nerd-icons-octicon
  :init
  (setq lsp-diagnostics-provider :flymake)

  (setq lsp-auto-guess-root nil
        lsp-imenu-index-function #'lsp-imenu-create-categorized-index

        lsp-signature-auto-activate t
        lsp-signature-render-documentation nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-diagnostics-scope :workspace
        lsp-modeline-workspace-status-enable nil
        lsp-headerline-breadcrumb-enable nil
        lsp-headerline-breadcrumb-segments '(symbols)

        ;; lsp-eslint-validate '(svelte typescript js javascript)
        lsp-semantic-tokens-enable nil  ;; do not color token by lsp
        lsp-progress-spinner-type 'progress-bar-filled

        lsp-keep-workspace-alive nil
        lsp-enable-indentation nil
        lsp-enable-folding nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-text-document-color nil
        lsp-enable-on-type-formatting nil
        lsp-enable-file-watchers nil
        lsp-file-watch-threshold 5000)
  :config
  ;; use emacs-lsp-booster
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

;;; python
(use-package lsp-pyright
  :init (setq lsp-pyright-venv-path (getenv "WORKON_HOME")))

;;; c/c++

(use-package lsp-c/c++
  :ensure nil
  :hook ((c-mode c-ts-mode c++-mode
                 c++-ts-mode cuda-mode) . (lambda () (lsp-deferred)))
  :init
  (setq lsp-clients-clangd-args
        '(
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
          )))

;;; java
(use-package lsp-java
  :demand t
  :hook ((java-mode java-ts-mode jdee-mode) . (lambda ()
                                                (require 'lsp-jt)
                                                (require 'lsp-java)
                                                (require 'dap-java)
                                                (lsp-deferred)
                                                (lsp-lens-mode 1)
                                                (lsp-java-lens-mode 1)
                                                (lsp-jt-lens-mode 1)))
  :init
  (setq
   lsp-java-boot-enabled nil  ;; disable boot-ls
   ;; Use Google style formatting by default
   ;; "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
   lsp-java-format-settings-url (lsp--path-to-uri
                                 (expand-file-name "eclipse-java-google-style.xml"
                                                   (concat user-emacs-directory "data")))
   lsp-java-format-settings-profile "GoogleStyle"
   ;; see https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones
   lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.35.0/jdt-language-server-1.35.0-202404251256.tar.gz"
   lsp-java-import-maven-enabled t
   lsp-java-implementations-code-lens-enabled t
   lsp-java-save-actions-organize-imports nil
   lsp-java-vmargs '("-XX:+UseParallelGC"
                     "-XX:GCTimeRatio=4"
                     "-XX:AdaptiveSizePolicyWeight=90"
                     "-Dsun.zip.disableMemoryMapping=true"
                     "-Xmx8G"
                     "-Xms100m")
   lsp-java-folding-range-enabled t)
  ;; latest jdtls requires java >= 17 to work
  (setq lsp-java-java-path "/opt/jdk17/bin/java")
  (require 'lsp-java-boot))

(use-package lsp-java-lombok
  :ensure nil
  :init
  (require 'lsp-java-lombok)
  (setq lsp-java-lombok/enabled t
        lsp-java-lombok/version "1.18.28"
        lsp-java-lombok/dir (expand-file-name "data/lsp-java-jars/" user-emacs-directory))
  (lsp-java-lombok/init))


(use-package lsp-metals :demand t)  ;; scala
(use-package consult-lsp)

;; (when (equal petmacs-lsp-mode-impl 'lsp-mode)
;;   (dolist (mode '(c-mode c-ts-mode c++-ts-mode
;;                          c++-mode cuda-mode
;;                          scala-mode scala-ts-mode
;;                          go-mode go-ts-mode))
;;     (add-hook mode #'lsp-deferred))
;;   ;;
;;   (dolist (mode '(java-mode java-ts-mode jdee-mode))
;;     (add-hook mode (lambda ()
;;                      (lsp-lens-mode 1)
;;                      (lsp-java-lens-mode 1)
;;                      (lsp-jt-lens-mode 1)
;;                      (lsp-deferred)))))

(provide 'init-lsp)
