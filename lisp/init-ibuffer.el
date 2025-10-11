;; init-buffer.el --- Initialize ibuffer configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; IBuffer configurations.
;;

;;; Code:

(use-package ibuffer
  :ensure nil
  :preface
  (defun petmacs/ibuffer-advance-motion (direction)
    (forward-line direction)
    (beginning-of-line)
    (if (not (get-text-property (point) 'ibuffer-filter-group-name))
        t
      (ibuffer-skip-properties '(ibuffer-filter-group-name)
                               direction)
      nil))

  (defun petmacs/ibuffer-previous-line (&optional arg)
    "Move backwards ARG lines, wrapping around the list if necessary."
    (interactive "P")
    (or arg (setq arg 1))
    (let (err1 err2)
      (while (> arg 0)
        (cl-decf arg)
        (setq err1 (petmacs/ibuffer-advance-motion -1)
              err2 (if (not (get-text-property (point) 'ibuffer-title))
                       t
                     (goto-char (point-max))
                     (beginning-of-line)
                     (ibuffer-skip-properties '(ibuffer-summary
                                                ibuffer-filter-group-name)
                                              -1)
                     nil)))
      (and err1 err2)))

  (defun petmacs/ibuffer-next-line (&optional arg)
    "Move forward ARG lines, wrapping around the list if necessary."
    (interactive "P")
    (or arg (setq arg 1))
    (let (err1 err2)
      (while (> arg 0)
        (cl-decf arg)
        (setq err1 (petmacs/ibuffer-advance-motion 1)
              err2 (if (not (get-text-property (point) 'ibuffer-summary))
                       t
                     (goto-char (point-min))
                     (beginning-of-line)
                     (ibuffer-skip-properties '(ibuffer-summary
                                                ibuffer-filter-group-name
                                                ibuffer-title)
                                              1)
                     nil)))
      (and err1 err2)))

  (defun petmacs/ibuffer-next-group ()
    (interactive)
    (while (petmacs/ibuffer-next-line)))

  (defun petmacs/ibuffer-previous-group ()
    (interactive)
    (while (petmacs/ibuffer-previous-line)))

  (defun petmacs/ibuffer-visit-buffer ()
    (interactive)
    (ibuffer-visit-buffer)
    (kill-buffer "*Ibuffer*"))

  :commands (ibuffer-find-file
             ibuffer-current-buffer)
  :bind ("C-x C-b" . ibuffer)
  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  :config
  (define-key ibuffer-mode-map (kbd "j") 'petmacs/ibuffer-next-line)
  (define-key ibuffer-mode-map (kbd "k") 'petmacs/ibuffer-previous-line)
  (define-key ibuffer-mode-map (kbd "J") 'petmacs/ibuffer-next-group)
  (define-key ibuffer-mode-map (kbd "K") 'petmacs/ibuffer-previous-group)
  (define-key ibuffer-mode-map (kbd "RET") 'petmacs/ibuffer-visit-buffer))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :init (setq nerd-icons-ibuffer-icon petmacs-icon))

;; Group ibuffer's list by project root
(use-package ibuffer-projectile
  :functions (nerd-icons-octicon icons-displayable-p)
  :hook ((ibuffer . (lambda ()
                      (ibuffer-projectile-set-filter-groups)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                        (ibuffer-do-sort-by-alphabetic)))))
  :config
  (setq ibuffer-projectile-prefix
        (if (icons-displayable-p)
            (concat
             (nerd-icons-octicon "nf-oct-file_directory_fill"
                                 :face ibuffer-filter-group-name-face
                                 :v-adjust -0.05
                                 :height 1.25)
             " ")
          "Project: ")))

(provide 'init-ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ibuffer.el ends here
