;; init-elisp.el --- Setup Emacs lisp IDE.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Show function arglist or variable docstring
;; `global-eldoc-mode' is enabled by default.
;; (use-package eldoc
;;   :ensure nil
;;   :diminish eldoc-mode)
(use-package eldoc
  :ensure nil
  :diminish
  :config
  (when emacs/>=26p
    ;; Display documentation in childframe
    (use-package eldoc-box
      :diminish
      :hook ((eldoc-mode . (lambda ()
                             ;; Compatible with `lsp-mode'
                             (unless (bound-and-true-p lsp-mode)
                               (eldoc-box-hover-mode 1)
                               (eldoc-box-hover-at-point-mode 1))))
             (lsp-mode . (lambda ()
                           ;; Compatible with `lsp-mode'
                           (if eldoc-box-hover-mode
                               (eldoc-box-hover-mode -1))))))))

;; Semantic code search for emacs lisp
(use-package elisp-refs)

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(provide 'init-elisp)

;;; init-elisp.el ends here
