;; init-yasnippet.el --- Setup yasnippet.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Integrate yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)


(provide 'init-yasnippet)

;;; init-yasnippet.el ends here
