;; init-lsp.el --- Setup lsp.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package lsp-mode
  ;; :pin melpa-stable
  :preface
  (defun petmacs//lsp-avy-document-symbol (all)
    (interactive)
    (let ((line 0) (col 0) (w (selected-window))
	  (ccls (memq major-mode '(c-mode c++-mode objc-mode)))
	  (start-line (1- (line-number-at-pos (window-start))))
	  (end-line (1- (line-number-at-pos (window-end))))
	  ranges point0 point1
	  candidates)
      (save-excursion
	(goto-char 1)
	(cl-loop for loc in
		 (lsp--send-request (lsp--make-request
				     "textDocument/documentSymbol"
				     `(:textDocument ,(lsp--text-document-identifier)
						     :all ,(if all t :json-false)
						     :startLine ,start-line :endLine ,end-line)))
		 for range = (if ccls loc (->> loc (gethash "location") (gethash "range")))
		 for range_start = (gethash "start" range)
		 for range_end = (gethash "end" range)
		 for l0 = (gethash "line" range_start)
		 for c0 = (gethash "character" range_start)
		 for l1 = (gethash "line" range_end)
		 for c1 = (gethash "character" range_end)
		 while (<= l0 end-line)
		 when (>= l0 start-line)
		 do
		 (forward-line (- l0 line))
		 (forward-char c0)
		 (setq point0 (point))
		 (forward-line (- l1 l0))
		 (forward-char c1)
		 (setq point1 (point))
		 (setq line l1 col c1)
		 (push `((,point0 . ,point1) . ,w) candidates)))
      ;; (require 'avy)
      (avy-with avy-document-symbol
	(avy--process candidates
		      (avy--style-fn avy-style)))))

  (defun petmacs/lsp-avy-goto-word ()
    (interactive)
    (petmacs//lsp-avy-document-symbol t))

  (defun petmacs/lsp-ui-doc-func ()
    "Toggle the function signature in the lsp-ui-doc overlay"
    (interactive)
    (setq lsp-ui-doc-include-signature (not lsp-ui-doc-include-signature)))

  (defun petmacs/lsp-ui-sideline-symb ()
    "Toggle the symbol in the lsp-ui-sideline overlay.
(generally redundant in C modes)"
     (interactive)
     (setq lsp-ui-sideline-show-symbol (not lsp-ui-sideline-show-symbol)))

  (defun petmacs/lsp-ui-sideline-ignore-duplicate ()
    "Toggle ignore duplicates for lsp-ui-sideline overlay"
    (interactive)
    (setq lsp-ui-sideline-ignore-duplicate (not lsp-ui-sideline-ignore-duplicate)))

  (defun petmacs/lsp-find-definition-other-window ()
    (interactive)
    (let ((pop-up-windows t))
      (pop-to-buffer (current-buffer) t))
    (lsp-find-definition))

  (defun petmacs/lsp-find-declaration-other-window ()
    (interactive)
    (let ((pop-up-windows t))
      (pop-to-buffer (current-buffer) t))
    (lsp-find-declaration))

  (defun petmacs/lsp-find-references-other-window ()
    (interactive)
    (let ((pop-up-windows t))
      (pop-to-buffer (current-buffer) t))
    (lsp-find-references))

  (defun petmacs/lsp-find-implementation-other-window ()
    (interactive)
    (let ((pop-up-windows t))
      (pop-to-buffer (current-buffer) t))
    (lsp-find-implementation))

  (defun petmacs/lsp-find-type-definition-other-window ()
    (interactive)
    (let ((pop-up-windows t))
      (pop-to-buffer (current-buffer) t))
    (lsp-find-type-definition))

  :commands lsp
  :diminish lsp-mode
  :bind (
	 :map lsp-mode-map
	 ("C-c C-d" . lsp-describe-thing-at-point))
  :init
  (setq lsp-auto-guess-root t		;; Detect project root
	lsp-prefer-flymake nil		;; Use lsp-ui and flycheck
	flymake-fringe-indicator-position 'right-fringe)
  :config
  (progn
    (require 'lsp-clients)))

(use-package lsp-ui
  ;; :pin melpa-stable
  :commands lsp-ui-doc-hide
  :custom-face (lsp-ui-doc-background ((t (:background ,(face-background 'tooltip)))))
  :hook (after-load-theme . (lambda ()
			      (set-face-attribute 'lsp-ui-doc-background nil
						  :background (face-background 'tooltip))))
  :bind (
	 :map lsp-ui-mode-map
	 ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	 ([remap xref-find-references] . lsp-ui-peek-find-references)
	 ("C-c u" . lsp-ui-imenu)

	 :map lsp-ui-peek-mode-map
	 ("h" . lsp-ui-peek--select-prev-file)
	 ("j" . lsp-ui-peek--select-next)
	 ("k" . lsp-ui-peek--select-prev)
	 ("l" . lsp-ui-peek--select-next-file)
	 )
  :hook (lsp-ui-imenu-mode . (lambda ()
			       (display-line-numbers-mode -1)
			       (hl-line-mode -1)))
  :init
  (setq lsp-ui-doc-enable t
	lsp-ui-peek-enable t
	lsp-ui-doc-use-webkit nil
	lsp-ui-doc-include-signature t
	lsp-ui-doc-position 'top
	;; lsp-ui-doc-position 'at-point
	lsp-ui-doc-border (face-foreground 'default)

	lsp-ui-sideline-enable nil
	lsp-ui-sideline-ignore-duplicate t)

  ;; (setq lsp-ui-imenu-enable t)
  (setq lsp-ui-flycheck-enable nil)	;; disable flycheck

  (evil-define-key 'normal lsp-ui-imenu-mode-map (kbd "q") 'lsp-ui-imenu--kill)
  (evil-define-key 'normal lsp-ui-imenu-mode-map (kbd "J") 'lsp-ui-imenu--next-kind)
  (evil-define-key 'normal lsp-ui-imenu-mode-map (kbd "K") 'lsp-ui-imenu--prev-kind)
  (evil-define-key 'normal lsp-ui-imenu-mode-map (kbd "<return>") 'lsp-ui-imenu--visit)
  (evil-define-key 'normal lsp-ui-imenu-mode-map (kbd "d") 'lsp-ui-imenu--view)
  :config
  (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

  ;; Reset `lsp-ui-doc-background' after loading theme
  (add-hook 'after-load-theme-hook
	    (lambda ()
	      (setq lsp-ui-doc-border (face-foreground 'default))
	      (set-face-background 'lsp-ui-doc-background
				   (face-background 'tooltip))))

  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; @see https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))

(use-package company-lsp
  ;; :pin melpa-stable
  :init (setq company-lsp-cache-candidates 'auto))

;; Debug
;; (use-package dap-mode
;;   :after lsp-mode
;;   :diminish
;;   :hook ((after-init . dap-mode)
;;          (dap-mode . dap-ui-mode)

;;          (python-mode . (lambda () (require 'dap-python)))
;;          (go-mode . (lambda () (require 'dap-go)))
;;          (java-mode . (lambda () (require 'dap-java)))
;;          ((c-mode c++-mode objc-mode swift) . (lambda () (require 'dap-lldb)))
;;          (php-mode . (lambda () (require 'dap-php)))))

;; `lsp-mode' and `treemacs' integration.
(use-package lsp-treemacs
  :bind (:map lsp-mode-map
	      ("M-9" . lsp-treemacs-errors-list)))

(provide 'init-lsp)

;;; init-lsp.el ends here
