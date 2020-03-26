;; init-sql.el --- Setup sql.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package sql
  :ensure nil
  :hook (sql-mode . (lambda ()
		      (set (make-local-variable 'company-backends)
			   '((company-capf company-files)))))
  )

(use-package sql-indent
  :quelpa
  (sql-indent :fetcher github
  	          :repo "alex-hhh/emacs-sql-indent"
  	          :files ("sql-indent.el"))
  :hook (sql-mode . sqlind-minor-mode))

(use-package sqlup-mode
  :hook ((sql-mode . sqlup-mode)
	 (sql-interactive-mode . sqlup-mode))
  :config
  (setq sqlup-blacklist (append sqlup-blacklist
                                sql-capitalize-keywords-blacklist)))

(provide 'init-sql)

;;; init-sql.el ends here
