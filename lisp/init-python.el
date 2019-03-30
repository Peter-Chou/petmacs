;; init-python.el --- Setup Python IDE.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-variable))

;; Python Mode
;; Install:
;;   pip install pyflakes
;;   pip install autopep8
(use-package python
  :ensure nil
  :defines gud-pdb-command-name pdb-path
  :config
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)

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
  (if sys/win32p
      (add-hook 'after-save-hook 'petmacs//python-sort-imports-windows)
    (add-hook 'before-save-hook 'petmacs//python-sort-imports))
  )

;; Emacs IPython Notebook
(use-package ein
  :diminish ein:notebook-mode
  :defines ein:completion-backend
  :init
  (setq ein:completion-backend 'ein:use-company-backend)

  ;; WORKAROUND:https://github.com/millejoh/emacs-ipython-notebook/issues/496
  (with-eval-after-load 'ido
    (defalias 'ido-completing-read 'completing-read)))

(use-package virtualenvwrapper)

(provide 'init-python)

;;; init-python.el ends here
