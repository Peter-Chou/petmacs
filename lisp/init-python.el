;; init-python.el --- Setup Python IDE.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Python Mode
;; Install:
;;   pip install yapf
;;   pip install isort
;;   pip install autoflake
(use-package python
  :ensure nil
  :hook
  ((python-mode . (lambda ()
		    (setq-local flycheck-checkers '(python-pylint))))
   (inferior-python-mode . (lambda ()
			     (process-query-on-exit-flag
			      (get-process "Python")))))
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)

  :config
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH"))

  (define-key inferior-python-mode-map (kbd "C-j") 'comint-next-input)
  (define-key inferior-python-mode-map (kbd "<up>") 'comint-next-input)
  (define-key inferior-python-mode-map (kbd "C-k") 'comint-previous-input)
  (define-key inferior-python-mode-map (kbd "<down>") 'comint-previous-input)
  (define-key inferior-python-mode-map
    (kbd "C-r") 'comint-history-isearch-backward))

(use-package py-isort)
(use-package pyvenv)
(use-package pipenv
  :commands (pipenv-activate
             pipenv-deactivate
             pipenv-shell
             pipenv-open
             pipenv-install
             pipenv-uninstall))
(use-package virtualenvwrapper)

;; Format using YAPF
;; Install: pip install yapf
(use-package yapfify
  :diminish yapf-mode
  :hook (python-mode . yapf-mode))

(if (member 'python-mode petmacs-lsp-active-modes)
    (progn
      (use-package lsp-python-ms
	:hook (python-mode . (lambda () (require 'lsp-python-ms)))
	;; :hook (python-mode . (lambda ()
	;; 		       (require 'lsp-python-ms)
	;; 		       (lsp-deferred)
	;; 		       (flycheck-mode -1)))
	:init
	(setq lsp-python-ms-nupkg-channel "stable")))
  (progn
    (use-package anaconda-mode
      :defines anaconda-mode-localhost-address
      :diminish anaconda-mode
      :hook ((python-mode . anaconda-mode)
	     (python-mode . anaconda-eldoc-mode))
      :config
      ;; WORKAROUND: https://github.com/proofit404/anaconda-mode#faq
      ;; (when (eq system-type 'darwin)
      ;;   (setq anaconda-mode-localhost-address "localhost"))
      (setq anaconda-mode-localhost-address "localhost"))

    (use-package company-anaconda
      :after company
      :defines company-backends
      :init (cl-pushnew 'company-anaconda company-backends)
      :config
      (evil-define-minor-mode-key 'normal 'anaconda-mode (kbd "C-M-i") 'company-anaconda)
      (evil-define-minor-mode-key 'insert 'anaconda-mode (kbd "C-M-i") 'company-anaconda))))

(provide 'init-python)

;;; init-python.el ends here
