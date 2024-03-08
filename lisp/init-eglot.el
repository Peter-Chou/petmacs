;; -*- lexical-binding: t no-byte-compile: t -*-

(setq read-process-output-max (* 5 1024 1024)) ;; 5mb

(with-no-warnings
  (use-package eglot
    :preface
    (defun petmacs/eglot-keybindgs ()
      (define-key evil-motion-state-map "gR" #'eglot-rename)
      (define-key evil-motion-state-map "gr" #'xref-find-references)
      (define-key evil-normal-state-map "gi" #'eglot-find-implementation)
      (define-key evil-motion-state-map "gh" #'eldoc)
      (define-key evil-normal-state-map "ga" #'eglot-code-actions))
    :init
    (setq eglot-send-changes-idle-time 0.2
          eglot-autoshutdown t
          eglot-connect-timeout 120
          eglot-ignored-server-capabilities '(:inlayHintProvider)
          eldoc-echo-area-use-multiline-p nil
          eglot-server-programs '(
                                  ((python-mode python-ts-mode) . ("pyright-langserver" "--stdio"))
                                  ((c++-mode c-mode c++-ts-mode c-ts-mode objc-mode) ("clangd"))
                                  ((cmake-mode cmake-ts-mode) . ("cmake-language-server"))
                                  ((java-mode java-ts-mode) . ("jdtls"))
                                  ((bash-ts-mode sh-mode) . ("bash-language-server" "start"))
                                  ((go-mode go-dot-mod-mode go-dot-work-mode go-ts-mode go-mod-ts-mode)
                                   . ("gopls"))
                                  ((yaml-ts-mode yaml-mode) . ("yaml-language-server" "--stdio"))
                                  ((dockerfile-mode dockerfile-ts-mode) . ("docker-langserver" "--stdio"))))
    (advice-add 'eglot-ensure :after 'petmacs/eglot-keybindgs)
    :config
    (push :documentHighlightProvider eglot-ignored-server-capabilities)))

(use-package consult-eglot)

(provide 'init-eglot)
