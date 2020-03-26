;; init-yaml.el --- Setup yaml.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package yaml-mode
  :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
         ("Procfile\\'" . yaml-mode))
  :hook (yaml-mode . (lambda ()
                       (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(provide 'init-yaml)

;;; init-yaml.el ends here
