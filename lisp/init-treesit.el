;; -*- lexical-binding: t no-byte-compile: t -*-

(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  (use-package treesit
    :ensure nil
    :init
    (setq treesit-font-lock-level 4
          major-mode-remap-alist
          '((c-mode          . c-ts-mode)
            (c++-mode        . c++-ts-mode)
            (cmake-mode        . cmake-ts-mode)
            (csharp-mode     . csharp-ts-mode)
            (conf-toml-mode  . toml-ts-mode)
            (css-mode        . css-ts-mode)
            (yaml-mode        . yaml-ts-mode)
            (java-mode       . java-ts-mode)
            (javascript-mode . js-ts-mode)
            (json-mode . json-ts-mode)
            (js-json-mode    . json-ts-mode)
            (python-mode     . python-ts-mode)
            (ruby-mode       . ruby-ts-mode)
            (sh-mode         . bash-ts-mode)))
    :config
    (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.jsonl\\'" . json-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))

  ;; M-x treesit-auto-install-all
  (use-package treesit-auto :demand t))

(provide 'init-treesit)
