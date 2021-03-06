;; init-dap.el --- Setup dap.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Debug
;; python: pip install "debugpy"
;; C++: apt-get install lldb nodejs npm
;; C++: follow instruction from https://github.com/llvm-mirror/lldb/tree/master/tools/lldb-vscode
;; `lsp-mode' and `treemacs' integration
(use-package dap-mode
  :diminish
  :defines dap-python-executable
  :diminish
  :bind (:map lsp-mode-map
         ("<f5>" . dap-debug)
         ("M-<f5>" . dap-hydra))
  :hook ((after-init . dap-auto-configure-mode)
         ;; (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
	 ((c-mode c++-mode objc-mode swift-mode) . (lambda () (require 'dap-lldb)))
         (python-mode . (lambda () (require 'dap-python)))
	 ;;;; go install github.com/go-delve/delve/cmd/dlv@latest
         (go-mode . (lambda () (require 'dap-go)))
         (java-mode . (lambda () (require 'dap-java)))
         ((js-mode js2-mode) . (lambda () (require 'dap-chrome)))
	 )
  :init
  (require 'cl-lib)
  (setq dap-enable-mouse-support t
	dap-auto-configure-features '(sessions locals controls tooltip repl)
	)
  :config
  (with-eval-after-load 'dap-ui
    (setq dap-ui-buffer-configurations
          `((,dap-ui--locals-buffer . ((side . right) (slot . 1) (window-width . 0.20)))
            (,dap-ui--expressions-buffer . ((side . right) (slot . 2) (window-width . 0.20)))
            (,dap-ui--sessions-buffer . ((side . right) (slot . 3) (window-width . 0.20)))
            (,dap-ui--breakpoints-buffer . ((side . left) (slot . 2) (window-width . ,treemacs-width)))
            (,dap-ui--debug-window-buffer . ((side . bottom) (slot . 3) (window-width . 0.20)))
            (,dap-ui--repl-buffer . ((side . bottom) (slot . 1) (window-width . 0.35))))))
  )

(provide 'init-dap)

;;; init-dap.el ends here
