;; -*- lexical-binding: t no-byte-compile: t -*-

;; M-x treesit-auto-install-all
(use-package treesit-auto
  :demand t
  :init
  (setq treesit-auto-install 'prompt
        treesit-font-lock-level 4)
  :config
  (global-treesit-auto-mode))

;; (use-package treesit
;;   :ensure nil
;;   :preface
;;   (defun install-ts-grammars (&optional force)
;;     "Install Tree-sitter grammars if they are absent."
;;     (interactive)
;;     (dolist (grammar
;;              '(
;;                ;; not sure where these ones are meant to come from, not at that url
;;                ;;(cmake  "https://github.com/tree-sitter/tree-sitter-cmake")
;;                ;;(go-mod  "https://github.com/tree-sitter/tree-sitter-go-mod")
;;                ;; these aren't yet part of emacs
;;                ;;(php  "https://github.com/tree-sitter/tree-sitter-php")
;;                ;;(html  "https://github.com/tree-sitter/tree-sitter-html")
;;                ;;(elisp  "https://github.com/tree-sitter/tree-sitter-elisp")
;;                ;;(swift  "https://github.com/tree-sitter/tree-sitter-swift")
;;                ;;(cli  "https://github.com/tree-sitter/tree-sitter-cli")
;;                (bash  "https://github.com/tree-sitter/tree-sitter-bash")
;;                (toml  "https://github.com/tree-sitter/tree-sitter-toml")
;;                (yaml  "https://github.com/tree-sitter/tree-sitter-yaml")
;;                (rust  "https://github.com/tree-sitter/tree-sitter-rust")
;;                (ruby  "https://github.com/tree-sitter/tree-sitter-ruby")
;;                (json  "https://github.com/tree-sitter/tree-sitter-json")
;;                (go  "https://github.com/tree-sitter/tree-sitter-go")
;;                (c "https://github.com/tree-sitter/tree-sitter-c")
;;                (cpp  "https://github.com/tree-sitter/tree-sitter-cpp")
;;                (css "https://github.com/tree-sitter/tree-sitter-css")
;;                (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
;;                (python "https://github.com/tree-sitter/tree-sitter-python")
;;                (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
;;                (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
;;       (add-to-list 'treesit-language-source-alist grammar)
;;       (if (or force (not (treesit-language-available-p (car grammar))))
;;           (treesit-install-language-grammar (car grammar)))))
;;   (dolist (mapping '((python-mode . python-ts-mode)
;;                      (css-mode . css-ts-mode)
;;                      (typescript-mode . tsx-ts-mode)
;;                      (js-mode . tsx-ts-mode)
;;                      (css-mode . css-ts-mode)
;;                      (yaml-mode . yaml-ts-mode)
;;                      (json-mode . json-ts-mode)
;;                      (ruby-mode . ruby-ts-mode)
;;                      (go-mode . go-ts-mode)))
;;     (add-to-list 'major-mode-remap-alist mapping))
;;   :config
;;   (install-ts-grammars))

(provide 'init-treesit)
