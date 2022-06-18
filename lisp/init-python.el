;; init-python.el --- Better default configurations.	-*- lexical-binding: t -*-

;; Install:
;;   pip install yapf
;;   pip install isort
;;   pip install autoflake
(use-package python
  :ensure nil
  :hook
  ((python-mode . (lambda ()
		            (setq-local flycheck-checkers '(python-pylint))
		            ;; (setq-local python-mode t)
                    (pyvenv-tracking-mode 1)
		            (pyvenv-mode 1)))
   (inferior-python-mode . (lambda ()
			                 (process-query-on-exit-flag
			                  (get-process "Python")))))
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)

  :config
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH"))

  (define-key inferior-python-mode-map (kbd "C-j") 'comint-next-input)
  (define-key inferior-python-mode-map (kbd "<up>") 'comint-next-input)
  (define-key inferior-python-mode-map (kbd "C-k") 'comint-previous-input)
  (define-key inferior-python-mode-map (kbd "<down>") 'comint-previous-input)
  (define-key inferior-python-mode-map
    (kbd "C-r") 'comint-history-isearch-backward))

(use-package py-isort)

(use-package pyvenv
  :preface
  ;; autoload virtual environment if project_root/pyrightconfig.json file exists,
  (defun petmacs/pyvenv-pyright-autoload ()
    (require 'projectile)
    (require 'json)
    (let* ((pdir (projectile-project-root))
           (pfile (concat pdir "pyrightconfig.json"))
           (json-object-type 'hash-table)
           (json-array-type 'string)
           (json-key-type 'string))
      (when (file-exists-p pfile)
        (setq-local pyvenv-workon (gethash "venv" (json-read-file pfile)))
        (pyvenv-workon pyvenv-workon)
        (if (equal petmacs-lsp-client-type 'lsp-mode)
            (lsp-deferred))
        )))

  :hook (python-mode . petmacs/pyvenv-pyright-autoload)
  )

(use-package virtualenvwrapper)

(use-package yapfify
  :diminish yapf-mode
  :hook (python-mode . yapf-mode))


;; Debug
;; python: pip install "debugpy"
;; install built llvm project to /opt/llvm
;; `lsp-mode' and `treemacs' integration
(use-package dap-mode
  :diminish
  :defines dap-python-executable
  :diminish
  :hook ((after-init . dap-auto-configure-mode)
         ;; (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
	 ;;; dap-lldb needs lldb-vscode which is in LLVM prebuilt package
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
	    dap-lldb-debug-program '("/opt/llvm/bin/lldb-vscode")
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


(provide 'init-python)
