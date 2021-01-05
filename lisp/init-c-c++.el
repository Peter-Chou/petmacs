;; init-c-c++.el --- Setup c/c++ IDE.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; install llvm-10 or above
;; Add the following code in your ~/.profile file and reboot
;; LLVM 10.0.0 is installed in /home/software/llvm
;; LLVM_HOME=/home/software/llvm
;; export PATH=$LLVM_HOME/bin:$PATH
;; export LD_LIBRARY_PATH=$LLVM_HOME/lib:$LD_LIBRARY_PATH
(use-package cc-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist
	       `("\\.h\\'" . ,petmacs-default-mode-for-headers))

  (setq lsp-clients-clangd-args
	'("-j=6" "-log=verbose" "-background-index"
	  "-cross-file-rename"
          ))

  ;; C/C++/Objective-C support
  ;; (use-package ccls
  ;;   :defines projectile-project-root-files-top-down-recurring
  ;;   :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls)))
  ;;   :init
  ;;   (setq ccls-executable (file-truename "~/ccls/Release/ccls"))
  ;;   :config
  ;;   (with-eval-after-load 'projectile
  ;;     (setq projectile-project-root-files-top-down-recurring
  ;;           (append '("compile_commands.json" ".ccls")
  ;;                   projectile-project-root-files-top-down-recurring))))

  :hook ((c-mode c++-mode) . (lambda ()
			       "Format and add/delete imports."
			       (add-hook 'before-save-hook #'lsp-format-buffer t t)
			       (add-hook 'before-save-hook #'lsp-organize-imports t t)
			       ;; enable lsp
			       (lsp-deferred)))
  :config
  (require 'compile))

;; C/C++/Objective-C support
;; (use-package ccls
;;   :defines projectile-project-root-files-top-down-recurring
;;   :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls)))
;;   :init
;;   (setq ccls-executable (file-truename "~/ccls/Release/ccls"))
;;   ;; :config
;;   ;; (with-eval-after-load 'projectile
;;   ;;   (setq projectile-project-root-files-top-down-recurring
;;   ;;         (append '("compile_commands.json" ".ccls")
;;   ;;                 projectile-project-root-files-top-down-recurring)))
;;   )

(use-package smart-semicolon
  :defer t
  :hook ((c-mode-common . smart-semicolon-mode)))

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))
  :config
  (add-hook 'cmake-mode-hook (lambda()
                               (add-to-list (make-local-variable 'company-backends)
                                            'company-cmake))))
(use-package google-c-style
  :init
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))

(provide 'init-c-c++)

;;; init-c-c++.el ends here
