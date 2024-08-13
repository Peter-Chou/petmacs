;; -*- lexical-binding: t no-byte-compile: t -*-

(eval-when-compile
  (require 'init-custom))

;; Debug
;; python: pip install "debugpy"
;; install built llvm project to /opt/llvm
;; `lsp-mode' and `treemacs' integration

(when (not (equal petmacs-lsp-mode-impl 'lsp-mode))
  (use-package lsp-mode)
  (use-package lsp-ui))

(use-package dap-mode
  :diminish
  :defines dap-python-executable
  :functions dap-hydra/nil
  :bind (:map lsp-mode-map
         ("<f5>" . dap-debug)
         ("M-<f5>" . dap-hydra))
  :hook ((after-init . dap-auto-configure-mode)
         (dap-session-created . global-display-line-numbers-mode)
         ;; (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
         ;; (dap-terminated . (lambda (_) (dap-hydra/nil)))
         ((python-mode python-ts-mode) . (lambda () (require 'dap-python)))
         ((java-mode java-ts-mode) . (lambda () (require 'dap-java)))
         ;; dap-lldb needs lldb-vscode which is in LLVM prebuilt package
         ((c-mode c-ts-mode c++-mode c++-ts-mode) . (lambda () (require 'dap-lldb)))
         ((go-mode go-ts-mode) . (lambda () (require 'dap-dlv-go)))
         ((objc-mode swift-mode) . (lambda () (require 'dap-lldb)))
         ((js-mode js2-mode js-ts-mode) . (lambda () (require 'dap-chrome))))
  :init
  (require 'cl-lib)
  (setq dap-enable-mouse-support t
        dap-python-debugger 'debugpy
        dap-ui-locals-expand-depth 2
        dap-ui-expressions-expand-depth 2
        ;; minimun debug ui, when spcific buffer when needed
        lsp-enable-dap-auto-configure nil
        ;; dap-auto-configure-features '(sessions locals tooltip)
	    dap-lldb-debug-program '("/opt/llvm/bin/lldb-vscode"))
  :config
  (defun petmacs/dap-disconnect (session)
    (unless petmacs-enable-display-line-numbers
      (global-display-line-numbers-mode 0)))
  (advice-add #'dap-disconnect :after #'petmacs/dap-disconnect)

  (dap-ui-mode 1)
  (with-eval-after-load 'dap-ui
    (setq dap-ui-buffer-configurations
          `((,dap-ui--locals-buffer . ((side . right) (slot . 1) (window-width . 0.20)))
            (,dap-ui--expressions-buffer . ((side . right) (slot . 2) (window-width . 0.20)))
            (,dap-ui--sessions-buffer . ((side . right) (slot . 3) (window-width . 0.20)))
            (,dap-ui--breakpoints-buffer . ((side . left) (slot . 2) (window-width . ,treemacs-width)))
            (,dap-ui--debug-window-buffer . ((side . bottom) (slot . 3) (window-width . 0.20)))
            (,dap-ui--repl-buffer . ((side . bottom) (slot . 1) (window-width . 0.35)))))))

(provide 'init-dap)
