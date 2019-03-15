;; init-company.el --- Setup company related packages.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-variable))

(use-package company
  :diminish company-mode
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-abort
  :bind (("M-/" . company-complete)
	 ("<backtab>" . company-yasnippet)
	 :map company-active-map
	 ("C-p" . company-select-previous)
	 ("C-n" . company-select-next)
	 ("<tab>" . company-complete-selection)
	 ("C-/" . counsel-company)
	 ;; ("C-M-/" . company-filter-candidates)
	 ("C-d" . company-show-doc-buffer)
	 :map company-search-map
	 ("C-p" . company-select-previous)
	 ("C-n" . company-select-next)
	 ;; ("C-/" . company-search-candidates)
	 ;; ("C-M-/" . company-filter-candidates)
	 ("C-d" . company-show-doc-buffer))
  :hook (after-init . global-company-mode)
  :config
  (setq company-tooltip-align-annotations t ; aligns annotation to the right
	company-tooltip-limit 12            ; bigger popup window
	company-idle-delay .1               ; decrease delay before autocompletion popup shows
	company-echo-delay 0                ; remove annoying blinking
	company-minimum-prefix-length 2
	company-require-match nil
	company-dabbrev-ignore-case nil
	company-dabbrev-downcase nil)
  (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete))

(when emacs/>=26p
  (use-package company-box
    :diminish
    :functions (all-the-icons-faicon
                all-the-icons-material
                all-the-icons-octicon
                all-the-icons-alltheicon)
    :hook (company-mode . company-box-mode)
    :init (setq company-box-enable-icon (display-graphic-p))
    :config
    (setq company-box-backends-colors nil)))

(provide 'init-company)

;;; init-company.el ends here
