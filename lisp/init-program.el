(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode 1)

(add-hook 'prog-mode-hook #'subword-mode)
(add-hook 'minibuffer-setup-hook #'subword-mode)

;; IDO
(ido-mode 1)
(ido-everywhere 1)
(setq ido-use-virtual-buffers t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-enable-flex-matching t)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to find a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

(use-package fill-column-indicator
  :hook (prog-mode . (lambda ()
		       (fci-mode 1)
		       (fci-update-all-windows t)))
  :init
    (setq fci-rule-color "#FFA631"
	fci-rule-use-dashes t))

(provide 'init-program)
