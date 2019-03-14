;; init-layout.el --- Setup layout related packages.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Windows/buffers sets shared among frames + save/load.
(use-package persp-mode
  :diminish
  :defines ivy-sort-functions-alist
  :commands (get-current-persp persp-contain-buffer-p)
  :hook (after-init . persp-mode)
  :init
  (setq persp-keymap-prefix (kbd "C-x p"))
  (setq persp-nil-name "default")
  (setq persp-set-last-persp-for-new-frames nil)
  (setq persp-kill-foreign-buffer-behaviour 'kill)
  (setq persp-auto-resume-time 0)
  (setq persp-common-buffer-filter-functions
        (list #'(lambda (b)
                  "Ignore temporary buffers."
                  (or (string-prefix-p " " (buffer-name b))
                      (string-prefix-p "*" (buffer-name b))
                      (string-prefix-p "magit" (buffer-name b))))))
  :config
  ;; Integrate IVY
  (with-eval-after-load "ivy"
    (add-hook 'ivy-ignore-buffers
              #'(lambda (b)
                  (when persp-mode
                    (let ((persp (get-current-persp)))
                      (if persp
                          (not (persp-contain-buffer-p b persp))
                        nil)))))

    (setq ivy-sort-functions-alist
          (append ivy-sort-functions-alist
                  '((persp-kill-buffer   . nil)
                    (persp-remove-buffer . nil)
                    (persp-add-buffer    . nil)
                    (persp-switch        . nil)
                    (persp-window-switch . nil)
                    (persp-frame-switch  . nil))))))

;; Integrate `projectile'
(use-package persp-mode-projectile-bridge
  :functions (persp-get-by-name
              persp-add-new set-persp-parameter
              persp-add-buffer projectile-project-buffers)
  :commands (persp-mode-projectile-bridge-find-perspectives-for-all-buffers
             persp-mode-projectile-bridge-kill-perspectives
             persp-mode-projectile-bridge-add-new-persp)
  :hook
  ((persp-mode . persp-mode-projectile-bridge-mode)
   (persp-mode-projectile-bridge-mode
    .
    (lambda ()
      (if persp-mode-projectile-bridge-mode
          (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
        (persp-mode-projectile-bridge-kill-perspectives)))))
  :init (setq persp-mode-projectile-bridge-persp-name-prefix "[p]")
  :config
  ;; HACK:Allow saving to files
  (eval-and-compile
    (defun my-persp-mode-projectile-bridge-add-new-persp (name)
      (let ((persp (persp-get-by-name name *persp-hash* :nil)))
        (if (eq :nil persp)
            (prog1
                (setq persp (persp-add-new name))
              (when persp
                (set-persp-parameter 'persp-mode-projectile-bridge t persp)
                (persp-add-buffer (projectile-project-buffers)
                                  persp nil nil)))
          persp)))
    (advice-add #'persp-mode-projectile-bridge-add-new-persp
                :override #'my-persp-mode-projectile-bridge-add-new-persp)))
(provide 'init-layout)

;;; init-layout.el ends here
