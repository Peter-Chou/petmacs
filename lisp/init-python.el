;; -*- lexical-binding: t no-byte-compile: t -*-

;; Install:
;;   pip install isort
;;   pip install ruff
(use-package python
  :ensure nil
  :functions exec-path-from-shell-copy-env
  :hook
  ((inferior-python-mode . (lambda ()
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


(use-package pyvenv
  :preface
  (defun petmacs/set-pythonpath-project ()
    "set project root path as PYTHONPATH"
    (if (not (getenv "PYTHONPATH"))
        ;; 没有时初始化
        (setenv "PYTHONPATH" (projectile-project-root))
      ;; 当前项目根目录不在PYTHONPATH里时
      (when (not (string-match (projectile-project-root) (getenv "PYTHONPATH")))
        (setenv "PYTHONPATH" (concat (getenv "PYTHONPATH") ":" (projectile-project-root))))))

  (defun petmacs/remove-pyvenv-modeline-env-info ()
    ;; (setq mode-line-misc-info (delete '(pyvenv-mode pyvenv-mode-line-indicator) mode-line-misc-info))
    (setq mode-line-misc-info
          (delq (assq 'pyvenv-mode mode-line-misc-info)
                mode-line-misc-info)))

  (defun petmacs/pyvenv-pyright-autoload ()
    "autoload virtual environment if project_root/pyrightconfig.json file exists,"
    (interactive)
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
        (pyvenv-workon pyvenv-workon))

      (petmacs/set-pythonpath-project)

      (pcase petmacs-lsp-mode-impl
        ('lsp-mode
         (lsp-deferred))
        ('eglot
         (eglot-ensure)))))
  :hook (((python-mode python-ts-mode) . petmacs/pyvenv-pyright-autoload)
         (pyvenv-mode . petmacs/remove-pyvenv-modeline-env-info))
  :config
  (pyvenv-mode 1)
  (pyvenv-tracking-mode 1))

(use-package sphinx-doc
  :hook ((python-mode python-ts-mode) . sphinx-doc-mode))

(use-package py-isort)
(use-package pip-requirements)
(use-package virtualenvwrapper)

(provide 'init-python)
