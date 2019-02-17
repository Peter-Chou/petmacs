
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
  (when (eq system-type 'darwin)
    (setq anaconda-mode-localhost-address "localhost")))

(use-package company-anaconda
  :after company
  :defines company-backends
  :init (cl-pushnew 'company-anaconda company-backends))

(provide 'init-python)
