;; init-dap.el --- Initialize DAP configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Debug Adapter Protocol (DAP) configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package dape
  :bind (("<f5>" . dape)
         ("M-<f5>" . dape-hydra/body))
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
  (setq dape-cwd-fn 'projectile-project-root
        dape-buffer-window-arrangment 'right)
  (require 'dape)
  :config
  (plist-put (alist-get 'debugpy dape-configs) 'command "python")
  ;; lldb-vscode renamed as lldb-dap after llvm 16
  (when (executable-find "lldb-dap")
    (plist-put (alist-get 'lldb-vscode dape-configs) 'command "lldb-dap"))

  (add-to-list 'dape-configs
               `(pytest-file
                 modes (python-ts-mode python-mode)
                 :module "pytest"
                 command "python"
                 command-args ("-m" "debugpy.adapter" "--host" "0.0.0.0" "--port" :autoport)
                 port :autoport
                 :request "launch"
                 :type "python"
                 :cwd dape-cwd
                 :justMyCode nil
                 :console "integratedTerminal"
                 :showReturnValue t
                 :args [dape-buffer-default]
                 :stopOnEntry nil
                 :cwd dape-cwd-fn))

  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-on-start-hooks 'dape-info)
  ;; (remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  ;; (add-hook 'dape-on-stopped-hooks 'dape-info)
  ;; (add-hook 'dape-on-stopped-hooks 'dape-repl)

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-compile-hooks 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-on-start-hooks (lambda () (save-some-buffers t t)))

  ;; ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-on-start-hooks
  ;;           (defun dape--save-on-start ()
  ;;             (save-some-buffers t t)))
  ;; ;; Display hydra on startup
  ;; (add-hook 'dape-on-start-hooks #'dape-hydra/body)
  )

(provide 'init-dap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dap.el ends here
