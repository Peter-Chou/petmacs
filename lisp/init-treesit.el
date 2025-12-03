;; init-treesit.el --- Initialize builtin tree sitter	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Tree Sitter.
;;

;;; Code:

(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  (use-package treesit
    :ensure nil
    :init
    (setq treesit-font-lock-level 4
          major-mode-remap-alist
          '((c-mode          . c-ts-mode)
            (c++-mode        . c++-ts-mode)
            (c-or-c++-mode   . c-or-c++-ts-mode)
            (java-mode       . java-ts-mode)
            (python-mode     . python-ts-mode)
            (cmake-mode      . cmake-ts-mode)
            (json-mode       . json-ts-mode)
            (js-json-mode    . json-ts-mode)
            (js-mode         . js-ts-mode)
            (javascript-mode . js-ts-mode)
            (js2-mode        . js-ts-mode)
            (css-mode        . css-ts-mode)
            (csharp-mode     . csharp-ts-mode)
            (conf-toml-mode  . toml-ts-mode)
            (yaml-mode       . yaml-ts-mode)
            (dockerfile-mode . dockerfile-ts-mode)
            (sh-mode         . bash-ts-mode)))
    :config
    (add-to-list 'auto-mode-alist '("\\.rs\\'"    . rust-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.ts\\'"    . typescript-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.jsonl\\'" . json-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.tsx\\'"   . tsx-ts-mode)))

  ;; M-x treesit-auto-install-all
  (use-package treesit-auto
    :demand t
    :init
    (setq treesit-auto-langs '(
                               bash c cmake cpp go gomod  javascript typescript
                               scala java python rust r vue make
                               json sql toml proto dockerfile yaml
                               bibtex css html org tsx lua))
    :config
    (treesit-auto-add-to-auto-mode-alist 'all))

  ;; native support to evil fold feature
  (use-package treesit-fold
    :pin nongnu
    :hook ((prog-mode . treesit-fold-mode)
           (treesit-fold-mode . treesit-fold-line-comment-mode))))

(provide 'init-treesit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-treesit.el ends here
