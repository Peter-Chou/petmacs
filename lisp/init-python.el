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
  :defines gud-pdb-command-name pdb-path
  :config
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)

  (define-key inferior-python-mode-map (kbd "C-j") 'comint-next-input)
  (define-key inferior-python-mode-map (kbd "<up>") 'comint-next-input)
  (define-key inferior-python-mode-map (kbd "C-k") 'comint-previous-input)
  (define-key inferior-python-mode-map (kbd "<down>") 'comint-previous-input)
  (define-key inferior-python-mode-map
    (kbd "C-r") 'comint-history-isearch-backward)

  (add-hook 'inferior-python-mode-hook
            (lambda ()
              ;; (bind-key "C-c C-z" #'kill-buffer-and-window inferior-python-mode-map)
              (process-query-on-exit-flag (get-process "Python")))))

(use-package pyvenv)

;; Format using YAPF
;; Install: pip install yapf
(use-package yapfify
  :diminish yapf-mode
  :hook (python-mode . yapf-mode))

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
  (evil-define-minor-mode-key 'insert 'anaconda-mode (kbd "C-M-i") 'company-anaconda))

(use-package py-isort
  :preface
  (defun petmacs//python-sort-imports ()
    ;; py-isort-before-save checks the major mode as well, however we can prevent
    ;; it from loading the package unnecessarily by doing our own check
    (when (and python-sort-imports-on-save
               (derived-mode-p 'python-mode))
      (py-isort-before-save)))

  (defun petmacs//python-sort-imports-windows ()
    "When in Python Mode, call isort on save"
    (when (eq major-mode 'python-mode)
      (shell-command-to-string (format "isort %s" buffer-file-name))))
  :init
  ;; Run on file save
  ;; (if sys/win32p
  ;;     (add-hook 'after-save-hook 'petmacs//python-sort-imports-windows)
  ;;   (add-hook 'before-save-hook 'petmacs//python-sort-imports))
  )

(use-package pipenv
  :commands (pipenv-activate
             pipenv-deactivate
             pipenv-shell
             pipenv-open
             pipenv-install
             pipenv-uninstall))

;; Emacs IPython Notebook
(use-package ein
  :diminish ein:notebook-mode)

(use-package virtualenvwrapper)

(provide 'init-python)

;;; init-python.el ends here
