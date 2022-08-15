;; -*- lexical-binding: t no-byte-compile: t -*-

;; Install:
;;   pip install yapf
;;   pip install isort
;;   pip install autoflake
(use-package python
  :ensure nil
  :hook
  ((python-mode . (lambda ()
		            (setq-local flycheck-checkers '(python-pylint))
                    ))
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

(use-package pyvenv
  :preface
  ;; autoload virtual environment if project_root/pyrightconfig.json file exists,
  (defun petmacs/pyvenv-pyright-autoload ()
    (require 'projectile)
    (require 'json)
    (let* ((pdir (projectile-project-root))
           (pfile (concat pdir "pyrightconfig.json"))
           (json-object-type 'hash-table)
           (json-array-type 'string)
           (json-key-type 'string))
      (when (file-exists-p pfile)
        ;; set pyvenv-workon buffer local variable for pyvenv-tracking-mode
        ;; to compare whether pyvenv-workon and pyvenv-virtual-env-name is equal
        (setq-local pyvenv-workon (gethash "venv" (json-read-file pfile)))
        (pyvenv-workon pyvenv-workon)
        ;; (cond ((equal petmacs-lsp-client-mode 'lsp-mode)
        ;;        (lsp-deferred))
        ;;       ((equal petmacs-lsp-client-mode 'lsp-bridge-mode)
        ;;        (lsp-bridge-restart-process)))
        )))
  :hook (python-mode . petmacs/pyvenv-pyright-autoload)
  :config
  (pyvenv-mode 1)
  (pyvenv-tracking-mode 1))

(use-package virtualenvwrapper)

(use-package yapfify
  :diminish yapf-mode
  :hook (python-mode . yapf-mode))

(use-package pip-requirements)

(provide 'init-python)
