
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; load custom-set-variables & custom-set-faces in custom file
(load-file custom-file)

(provide 'init-custom)
