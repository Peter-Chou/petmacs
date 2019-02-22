;; init-elisp.el --- Setup Emacs lisp IDE.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Show function arglist or variable docstring
;; `global-eldoc-mode' is enabled by default.
(use-package eldoc
  :ensure nil
  :diminish eldoc-mode)

;; Semantic code search for emacs lisp
(use-package elisp-refs)

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(provide 'init-elisp)

;;; init-elisp.el ends here