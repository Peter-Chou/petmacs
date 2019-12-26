;; init-layout.el --- Setup layout related packages.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package persp-mode
  :diminish
  :defines (recentf-exclude ivy-ignore-buffers ivy-sort-functions-alist)
  :commands (get-current-persp persp-contain-buffer-p)
  :hook ((after-init . persp-mode)
         (window-setup . toggle-frame-maximized))
  :init (setq persp-keymap-prefix (kbd "C-x p")
              persp-nil-name "default"
              persp-set-last-persp-for-new-frames nil
              persp-kill-foreign-buffer-behaviour 'kill
              persp-auto-resume-time 0
              persp-common-buffer-filter-functions
              (list #'(lambda (b)
                        "Ignore temporary buffers."
                        (let ((bname (file-name-nondirectory (buffer-name b))))
                          (or (string-prefix-p " " bname)
                              (and (string-prefix-p "*" bname)
                                   (not (string-equal "*scratch*" bname)))
                              (string-suffix-p ".elc" bname)
                              (string-suffix-p ".gz" bname)
                              (string-suffix-p ".zip" bname)
                              (string-prefix-p "Pfuture-Callback" bname)
                              (string-prefix-p "magit" bname)
                              (eq (buffer-local-value 'major-mode b) 'erc-mode)
                              (eq (buffer-local-value 'major-mode b) 'rcirc-mode)
                              (eq (buffer-local-value 'major-mode b) 'nov-mode)
                              (eq (buffer-local-value 'major-mode b) 'vterm-mode))))))
  :config
  ;; Don't save persp configs in `recentf'
  (push persp-save-dir recentf-exclude)

  ;; Integrate IVY
  (with-eval-after-load 'ivy
    (add-to-list 'ivy-ignore-buffers
                 #'(lambda (b)
                     (when persp-mode
                       (let ((persp (get-current-persp)))
                         (if persp
                             (not (persp-contain-buffer-p b persp))
                           nil)))))))

;; Projectile integration
(use-package persp-mode-projectile-bridge
  :after projectile
  :commands (persp-mode-projectile-bridge-find-perspectives-for-all-buffers
             persp-mode-projectile-bridge-kill-perspectives)
  :hook ((persp-mode . persp-mode-projectile-bridge-mode)
         (persp-mode-projectile-bridge-mode
          .
          (lambda ()
            (if persp-mode-projectile-bridge-mode
                (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
              (persp-mode-projectile-bridge-kill-perspectives)))))
  :init (setq persp-mode-projectile-bridge-persp-name-prefix "[p]")
  :config
  ;; HACK: Allow saving to files
  (with-no-warnings
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
