;; init-basic.el --- Setup basic  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Garbage Collector Magic Hack
(use-package gcmh
  :diminish
  :init
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold #x1000000) ; 16MB
  (gcmh-mode 1))


;; use exec-path-from-shell in linux / mac
(when (or (eq system-type 'gnu/linux) (eq system-type 'darwin) (daemonp))
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH"
					   "WORKON_HOME" "JAVA_HOME"
					   "LLVM_HOME" "LD_LIBRARY_PATH")
	  exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

(provide 'init-basic)

;;; init-basic.el ends here