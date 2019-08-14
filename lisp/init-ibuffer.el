;; init-ibuffer.el --- Setup ibuffer.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

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

  :functions (all-the-icons-icon-for-file
              all-the-icons-icon-for-mode
              all-the-icons-auto-mode-match?
              all-the-icons-faicon)
  :commands (ibuffer-find-file
             ibuffer-current-buffer)
  :bind ("C-x C-b" . ibuffer)
  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  :config
  (define-key ibuffer-mode-map (kbd "j") 'petmacs/ibuffer-next-line)
  (define-key ibuffer-mode-map (kbd "k") 'petmacs/ibuffer-previous-line)
  (define-key ibuffer-mode-map (kbd "J") 'petmacs/ibuffer-next-group)
  (define-key ibuffer-mode-map (kbd "K") 'petmacs/ibuffer-previous-group)
  (define-key ibuffer-mode-map (kbd "RET") 'petmacs/ibuffer-visit-buffer)

  ;; Display buffer icons on GUI
  (when (display-graphic-p)
    ;; To be correctly aligned, the size of the name field must be equal to that
    ;; of the icon column below, plus 1 (for the tab I inserted)
    (define-ibuffer-column icon (:name "  ")
      (let ((icon (if (and (buffer-file-name)
			   (all-the-icons-auto-mode-match?))
		      (all-the-icons-icon-for-file (file-name-nondirectory (buffer-file-name)) :v-adjust -0.05)
		    (all-the-icons-icon-for-mode major-mode :v-adjust -0.05))))
	(if (symbolp icon)
	    (setq icon (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0))
	  icon)))

    (setq ibuffer-formats `((mark modified read-only ,(if emacs/>=26p 'locked "")
                                  ;; Here you may adjust by replacing :right with :center or :left
                                  ;; According to taste, if you want the icon further from the name
                                  " " (icon 2 2 :left :elide)
                                  ,(propertize " " 'display `(space :align-to 8))
                                  (name 18 18 :left :elide)
                                  " " (size 9 -1 :right)
                                  " " (mode 16 16 :left :elide) " " filename-and-process)
                            (mark " " (name 16 -1) " " filename))))

  (with-eval-after-load 'counsel
    (defun my-ibuffer-find-file ()
      (interactive)
      (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                 (if (buffer-live-p buf)
                                     (with-current-buffer buf
                                       default-directory)
                                   default-directory))))
        (counsel-find-file default-directory)))
    (advice-add #'ibuffer-find-file :override #'my-ibuffer-find-file))

  ;; Group ibuffer's list by project root
  (use-package ibuffer-projectile
    :functions all-the-icons-octicon ibuffer-do-sort-by-alphabetic
    :hook ((ibuffer . (lambda ()
                        (ibuffer-projectile-set-filter-groups)
                        (unless (eq ibuffer-sorting-mode 'alphabetic)
                          (ibuffer-do-sort-by-alphabetic)))))
    :config
    (setq ibuffer-projectile-prefix
          (if (display-graphic-p)
              (concat
               (all-the-icons-octicon "file-directory"
                                      :face ibuffer-filter-group-name-face
                                      :v-adjust -0.05
                                      :height 1.25)
               " ")
            "Project: "))))

(provide 'init-ibuffer)

;;; init-ibuffer.el ends here
