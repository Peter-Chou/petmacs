;; -*- lexical-binding: t no-byte-compile: t -*-

(use-package mvn
  :preface
  (defun petmacs/mvn-clean-compile ()
    "Recompile using maven."
    (interactive)
    (mvn-clean)
    (mvn-compile)))

(use-package maven-test-mode
  :init (require 'maven-test-mode))

(provide 'init-java)
