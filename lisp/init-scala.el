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
  :mode "\\.s\\(cala\\|bt\\)$")

(provide 'init-scala)

;;; init-scala.el ends here
