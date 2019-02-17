
(use-package ibuffer-projectile
  :bind ("C-x C-b" . ibuffer)
  :hook ((ibuffer . (lambda ()
                      (ibuffer-projectile-set-filter-groups)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                        (ibuffer-do-sort-by-alphabetic)))))
  :config
  (setq ibuffer-projectile-prefix "Project: ")
  (setq ibuffer-filter-group-name-face 'font-lock-function-name-face)

  (with-eval-after-load 'counsel
    (defun petmacs//ibuffer-find-file (file &optional wildcards)
      "Like `find-file', but default to the directory of the buffer at point."
      (interactive
       (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                  (if (buffer-live-p buf)
                                      (buffer-local-value 'default-directory buf)
                                    default-directory))))
         (counsel-find-file))))
    (advice-add #'ibuffer-find-file :override #'petmacs//ibuffer-find-file)))

(provide 'init-ibuffer)
