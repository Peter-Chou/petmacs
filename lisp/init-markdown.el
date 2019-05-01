
(use-package markdown-mode
  :preface
  ;; stolen from http://stackoverflow.com/a/26297700
  ;; makes markdown tables saner via orgtbl-mode
  (defun petmacs//cleanup-org-tables ()
    (require 'org-table)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "-+-" nil t) (replace-match "-|-"))))

  (defun petmacs//cleanup-org-tables-on-save ()
    (add-hook 'before-save-hook 'petmacs//cleanup-org-tables nil 'local))
  :mode
  (("\\.m[k]d" . markdown-mode)
   ("\\.mdk" . markdown-mode))
  :hook ((markdown-mode . orgtbl-mode)
	  (markdown-mode . petmacs//cleanup-org-tables-on-save))
  :config
  (dolist (s '(normal insert))
    (evil-define-key s markdown-mode-map
      (kbd "M-h") 'markdown-promote
      (kbd "M-j") 'markdown-move-down
      (kbd "M-k") 'markdown-move-up
      (kbd "M-l") 'markdown-demote)))

(use-package markdown-toc)

(provide 'init-markdown)
