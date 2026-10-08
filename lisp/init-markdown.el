;; init-markdown.el --- Initialize markdown configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Markdown configurations.
;;

;;; Code:

;; (if (and emacs/>=31p (fboundp 'treesit-available-p)
;;          (treesit-available-p))
;;     (use-package markdown-ts-mode
;;       :ensure nil
;;       :mode ("\\.md\\'" "\\.mdx\\'" "\\.markdown\\'")
;;       :config
;;       (require 'markdown-ts-mode-x))
;;   ;; M-x treesit-install-language-grammar twice for libtree-sitter-markdown.so & libtree-sitter-markdown-inline.so
;;   (use-package markdown-ts-mode
;;     :mode ("\\.md\\'" "\\.mdx\\'" "\\.markdown\\'")
;;     :config
;;     (add-to-list 'treesit-language-source-alist '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
;;     (add-to-list 'treesit-language-source-alist '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src"))))

;; Preview markdown files
;; @see https://github.com/seagle0128/grip-mode?tab=readme-ov-file#prerequisite
(use-package grip-mode
  :defines markdown-mode-command-map org-mode-map grip-update-after-change grip-use-mdopen
  :functions auth-source-user-and-password
  :autoload grip-mode
  :init
  (with-eval-after-load 'markdown-mode
    (bind-key "C-g" #'grip-mode markdown-mode-command-map))

  (with-eval-after-load 'markdown-ts-mode
    (bind-key "C-c C-g" #'grip-mode markdown-ts-mode-map))

  (with-eval-after-load 'org
    (bind-key "C-c C-g" #'grip-mode org-mode-map))

  (setq grip-update-after-change nil)

  ;; mdopen doesn't need credentials, and only support external browsers
  (if (executable-find "mdopen")
      (setq grip-use-mdopen t)
    (when-let* ((credential (and (require 'auth-source nil t)
                                 (auth-source-user-and-password "api.github.com"))))
      (setq grip-github-user (car credential)
            grip-github-password (cadr credential)))))

(provide 'init-markdown)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-markdown.el ends here
