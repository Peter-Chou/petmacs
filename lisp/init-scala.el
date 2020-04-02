;; init-scala.el --- Setup scala IDE.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; please install metals-emacs -> https://scalameta.org/metals/docs/editors/emacs.html
(use-package scala-mode
  ;; lsp-metals is a part of lsp-mode
  :hook (scala-mode . (lambda ()
			(require 'lsp-metals)
			(lsp-deferred)))
  :mode "\\.s\\(cala\\|bt\\)$"
  :config
  (with-eval-after-load 'lsp-treemacs
    (lsp-metals-treeview-enable t)
    (setq lsp-metals-treeview-show-when-views-received t)))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false"))
)

(provide 'init-scala)

;;; init-scala.el ends here
