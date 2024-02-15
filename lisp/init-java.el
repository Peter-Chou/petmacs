;; -*- lexical-binding: t no-byte-compile: t -*-

;; maven uses JAVA_HOME to find which java version to run
;; e.g. JAVA_HOME=/opt/jdk8 mvn test
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
