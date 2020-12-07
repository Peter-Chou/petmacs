;; init-web.el --- Setup web configurations.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(use-package css-mode
  :ensure nil
  :init (setq css-indent-offset 2))

;; SCSS mode
(use-package scss-mode
  :init
  ;; Disable complilation on save
  (setq scss-compile-at-save nil))

;; New `less-css-mde' in Emacs 26
(unless (fboundp 'less-css-mode)
  (use-package less-css-mode))

;; CSS eldoc
(use-package css-eldoc
  :commands turn-on-css-eldoc
  :hook ((css-mode scss-mode less-css-mode) . turn-on-css-eldoc))

;; JSON mode
(use-package json-mode)

;; sudo apt-get install libxml2-utils  // for xmllint
(use-package xml-format
  :after nxml-mode)

;; JavaScript
(use-package js2-mode
  :defines flycheck-javascript-eslint-executable
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter (("node" . js2-mode)
                ("node" . js2-jsx-mode))
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-highlight-unused-variables-mode))
  :config

  (with-eval-after-load 'flycheck
    (when (or (executable-find "eslint_d")
              (executable-find "eslint")
              (executable-find "jshint"))
      (setq js2-mode-show-strict-warnings nil))
    (when (executable-find "eslint_d")
      ;; https://github.com/mantoni/eslint_d.js
      ;; npm -i -g eslint_d
      (setq flycheck-javascript-eslint-executable "eslint_d"))))

(use-package js2-refactor
  :diminish
  :hook (js2-mode . js2-refactor-mode)
  :config (js2r-add-keybindings-with-prefix "C-c C-m"))

;; Live browser JavaScript, CSS, and HTML interaction
(use-package skewer-mode
  :diminish
  :hook (((js-mode js2-mode). skewer-mode)
         (css-mode . skewer-css-mode)
         (web-mode . skewer-html-mode)
         (html-mode . skewer-html-mode))
  :init
  ;; diminish
  (with-eval-after-load 'skewer-css
    (diminish 'skewer-css-mode))
  (with-eval-after-load 'skewer-html
    (diminish 'skewer-html-mode)))

;; Typescript
(use-package typescript-mode)

;; Run Mocha or Jasmine tests
(use-package mocha
  :config (use-package mocha-snippets))

;; Major mode for editing web templates
(use-package web-mode
  :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

;; Format HTML, CSS and JavaScript/JSON
;; Install: npm -g install prettier
(use-package prettier-js
  :hook ((js-mode js2-mode json-mode web-mode css-mode sgml-mode html-mode)
         .
         prettier-js-mode))

(use-package haml-mode)
(use-package php-mode)

;; REST
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :config
  (use-package restclient-test
    :diminish
    :hook (restclient-mode . restclient-test-mode))

  (with-eval-after-load 'company
    (use-package company-restclient
      :defines company-backends
      :init (add-to-list 'company-backends 'company-restclient))))

(provide 'init-web)

;;; init-web.el ends here
