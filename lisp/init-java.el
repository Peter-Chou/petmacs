;; init-java.el --- Setup java IDE.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; latest jdtls requires java >= 11 to work
(use-package lsp-java
  :hook (java-mode . (lambda ()
		       (require 'lsp-java)
		       (lsp-deferred)))
  :init
  (setq lsp-java-import-maven-enabled t
	lsp-java-implementations-code-lens-enabled t
	lsp-java-save-actions-organize-imports t
	;; lsp-java-java-path "/opt/jdk11/bin/java"
	lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx6G" "-Xms100m")
	;; Runtime name must be one of: “J2SE-1.5”, “JavaSE-1.6”, “JavaSE-1.7”, “JavaSE-1.8” etc
	;; lsp-java-configuration-runtimes '[(:name "JavaSE-1.8"
	;; 				   :path "/opt/jdk/")
	;; 				  (:name "JavaSE-11"
	;; 				   :path "/opt/jdk11/"
	;; :default t)]
	lsp-java-folding-range-enabled t)
  )

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
