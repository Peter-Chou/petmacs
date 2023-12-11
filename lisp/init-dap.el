;; -*- lexical-binding: t no-byte-compile: t -*-

(require 'init-custom)

;; Debug
;; python: pip install "debugpy"
;; install built llvm project to /opt/llvm
;; `lsp-mode' and `treemacs' integration

(cond ((and (equal petmacs-dap-mode-impl 'dape) emacs/>=29p)
       (use-package dape
         :bind (("<f5>" . dape)
                ("M-<f5>" . dape-hydra/body))
         :custom (dape-buffer-window-arrangment 'right)
         :pretty-hydra
         ((:title (pretty-hydra-title "Debug" 'codicon "nf-cod-debug")
           :color pink :quit-key ("q" "C-g"))
          ("Stepping"
           (("n" dape-next "next")
            ("s" dape-step-in "step in")
            ("o" dape-step-out "step out")
            ("c" dape-continue "continue")
            ("p" dape-pause "pause")
            ("k" dape-kill "kill")
            ("r" dape-restart "restart")
            ("D" dape-disconnect-quit "disconnect"))
           "Switch"
           (("m" dape-read-memory "memory")
            ("t" dape-select-thread "thread")
            ("w" dape-watch-dwim "watch")
            ("S" dape-select-stack "stack")
            ("i" dape-info "info")
            ("R" dape-repl "repl"))
           "Breakpoints"
           (("b" dape-breakpoint-toggle "toggle")
            ("l" dape-breakpoint-log "log")
            ("e" dape-breakpoint-expression "expression")
            ("B" dape-breakpoint-remove-all "clear"))
           "Debug"
           (("d" dape "dape")
            ("Q" dape-quit "quit" :exit t))))
         :init
         (setq dape-cwd-fn 'projectile-project-root)
         :config
         ;; Save buffers on startup, useful for interpreted languages
         (add-hook 'dape-on-start-hooks
                   (defun dape--save-on-start ()
                     (save-some-buffers t t)))
         ;; Display hydra on startup
         (add-hook 'dape-on-start-hooks #'dape-hydra/body)))
      (t
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
               ;; minimun debug ui, when spcific buffer when needed
               lsp-enable-dap-auto-configure nil
               ;; dap-auto-configure-features '(sessions locals tooltip)
	           dap-lldb-debug-program '("/opt/llvm/bin/lldb-vscode"))
         :config
         (dap-ui-mode 1)
         (with-eval-after-load 'dap-ui
           (setq dap-ui-buffer-configurations
                 `((,dap-ui--locals-buffer . ((side . right) (slot . 1) (window-width . 0.20)))
                   (,dap-ui--expressions-buffer . ((side . right) (slot . 2) (window-width . 0.20)))
                   (,dap-ui--sessions-buffer . ((side . right) (slot . 3) (window-width . 0.20)))
                   (,dap-ui--breakpoints-buffer . ((side . left) (slot . 2) (window-width . ,treemacs-width)))
                   (,dap-ui--debug-window-buffer . ((side . bottom) (slot . 3) (window-width . 0.20)))
                   (,dap-ui--repl-buffer . ((side . bottom) (slot . 1) (window-width . 0.35)))))))
       ))

(provide 'init-dap)
