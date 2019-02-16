
;; Show function arglist or variable docstring
;; `global-eldoc-mode' is enabled by default.
(use-package eldoc
  :ensure nil
  :diminish eldoc-mode)

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(provide 'init-elisp)
