;; -*- lexical-binding: t no-byte-compile: t -*-

(setq read-process-output-max (* 5 1024 1024)) ;; 5mb

(use-package eglot
  :ensure t
  :init
  (setq eglot-send-changes-idle-time 0.2
        eldoc-echo-area-use-multiline-p nil
        eglot-autoshutdown t
        eglot-connect-timeout 120
        eglot-ignored-server-capabilities '(:inlayHintProvider)
        eldoc-echo-area-use-multiline-p nil
        )
  :config
  (push :documentHighlightProvider eglot-ignored-server-capabilities)
  )

(use-package consult-eglot
  :ensure t)


(provide 'init-eglot)
