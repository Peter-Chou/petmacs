;; init-dap.el --- Setup dap.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Debug
;; python: pip install "ptvsd>=4.2"
;; C++: apt-get install lldb nodejs npm
;; C++: follow instruction from https://github.com/llvm-mirror/lldb/tree/master/tools/lldb-vscode
;; `lsp-mode' and `treemacs' integration
(use-package dap-mode
  :preface
  (defun petmacs--autoload-dap-templates()
    "autoload dap templates in projectile_root_dir/dap_templates.el"
    (interactive)
    (let* ((pdir (projectile-project-root))
	   (pfile (concat pdir "dap_templates.el")))
      (when (file-exists-p pfile)
	(with-temp-buffer
	  (insert-file-contents pfile)
	  (eval-buffer)
	  (message "dap templates has been loaded.")))))
  (defun petmacs/register-dap-degbug-template ()
    (interactive)
    (require 'dap-python)
    (dap-register-debug-template "python-debug"
				 (list :type "python"
                                       :args "-i"
                                       :cwd nil
                                       :env '(("DEBUG" . "1"))
                                       :target-module nil
                                       :request "launch"
                                       :name "python-debug")))
  :diminish
  :defines dap-python-executable
  :functions dap-hydra/nil
  :diminish
  :bind (:map lsp-mode-map
         ("<f5>" . dap-debug)
         ("M-<f5>" . dap-hydra))
  :hook ((after-init . dap-auto-configure-mode)
         (dap-stopped . (lambda (_args) (dap-hydra)))
         (dap-terminated . (lambda (_args) (dap-hydra/nil)))
	 ((c-mode c++-mode objc-mode swift-mode) . (lambda () (require 'dap-lldb)))
         ;; (dap-session-created . (lambda (_args) (dap-hydra)))
         ;; (dap-stopped . (lambda (_args) (dap-hydra)))

         (python-mode . (lambda () (require 'dap-python)))
         (go-mode . (lambda () (require 'dap-go)))
         (java-mode . (lambda () (require 'dap-java)))
         ;; ((js-mode js2-mode) . (lambda () (require 'dap-chrome)))
	 ;;((c-mode c++-mode objc-mode swift-mode) . (lambda () (require 'dap-gdb-lldb))))
	 )
  :init
  (require 'cl-lib)
  (setq dap-enable-mouse-support t))

(provide 'init-dap)

;;; init-dap.el ends here
