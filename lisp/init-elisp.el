;; init-elisp.el --- Setup Emacs lisp IDE.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Show function arglist or variable docstring
;; `global-eldoc-mode' is enabled by default.
(use-package eldoc
  :ensure nil
  :diminish
  :config
  (when emacs/>=26p
    ;; Display documentation in childframe
    (use-package eldoc-box
      :diminish
      :hook ((eldoc-mode . eldoc-box-hover-mode)
             (eldoc-box-hover-mode . eldoc-box-hover-at-point-mode))
      :config
      ;; Compatible with `lsp-mode'
      (with-eval-after-load 'lsp-mode
        (add-hook 'lsp-mode-hook (lambda ()
                                   (eldoc-box-hover-mode -1)))))))

;; Semantic code search for emacs lisp
(use-package elisp-refs)

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(provide 'init-elisp)

;;; init-elisp.el ends here
