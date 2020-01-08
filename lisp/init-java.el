;; init-java.el --- Setup java IDE.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(use-package lsp-java
  :hook (java-mode . (lambda ()
		       (require 'lsp-java)
		       (lsp-deferred))))

(use-package mvn
  :preface
  (defun petmacs/mvn-clean-compile ()
    "Recompile using maven."
    (interactive)
    (mvn-clean)
    (mvn-compile)))

(use-package maven-test-mode)

(provide 'init-java)

;;; init-java.el ends here
