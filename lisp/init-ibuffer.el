
(use-package ibuffer
  :ensure nil
  :functions (all-the-icons-icon-for-buffer
              all-the-icons-icon-for-mode
              all-the-icons-icon-family)
  :commands (ibuffer-current-buffer
             ibuffer-find-file
             ibuffer-do-sort-by-alphabetic)
  :bind ("C-x C-b" . ibuffer)
  :init
  (setq ibuffer-filter-group-name-face '(:inherit (success bold))))

;; Display buffer icons on GUI
(when (display-graphic-p)
  (define-ibuffer-column icon (:name " ")
    (let ((icon (all-the-icons-icon-for-buffer)))
      (if (symbolp icon)
          (setq icon (all-the-icons-icon-for-mode 'fundamental-mode)))
      (unless (symbolp icon)
        (propertize icon
                    'face `(
                            :height 1.1
                            :family ,(all-the-icons-icon-family icon)
                            :inherit
                            )))))

  (setq ibuffer-formats '((mark modified read-only locked
                                " " (icon 2 2 :left :elide) (name 18 18 :left :elide)
                                " " (size 9 -1 :right)
                                " " (mode 16 16 :left :elide) " " filename-and-process)
                          (mark " " (name 16 -1) " " filename))))

(use-package ibuffer-projectile
  :bind ("C-x C-b" . ibuffer)
  :hook ((ibuffer . (lambda ()
                      (ibuffer-projectile-set-filter-groups)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                        (ibuffer-do-sort-by-alphabetic)))))
  :config
  (setq ibuffer-projectile-prefix "Project: ")
  (setq ibuffer-filter-group-name-face 'font-lock-function-name-face)
  (setq ibuffer-projectile-prefix
        (if (display-graphic-p)
            (concat
             (all-the-icons-octicon "file-directory"
                                    :face ibuffer-filter-group-name-face
                                    :v-adjust -0.04
                                    :height 1.1)
             " ")
          "Project: "))

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
