;; -*- lexical-binding: t no-byte-compile: t -*-

(require 'init-const)
(require 'init-funcs)

(use-package corfu
  :bind (:map corfu-map
         ("C-M-m" . corfu-move-to-minibuffer))
  :init
  (setq corfu-cycle t
        corfu-auto t
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match 'separator
        corfu-preview-current nil
        corfu-preselect-first t
        corfu-auto-delay 0.1
        corfu-auto-prefix 1
        )
  (when (> (frame-pixel-width) 3000) (custom-set-faces '(corfu-default ((t (:height 1.3))))))
  (global-corfu-mode)

  (defun corfu-beginning-of-prompt ()
    "Move to beginning of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (car completion-in-region--data)))

  (defun corfu-end-of-prompt ()
    "Move to end of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (cadr completion-in-region--data)))

  (define-key corfu-map [remap move-beginning-of-line] #'corfu-beginning-of-prompt)
  (define-key corfu-map [remap move-end-of-line] #'corfu-end-of-prompt)

  (with-eval-after-load 'lsp-mode
    (setq lsp-completion-provider :none) ;; we use Corfu!

    (defun petmacs/lsp-mode-setup-completion ()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless))) ;; Configure orderless

    (add-hook 'lsp-completion-mode-hook #'petmacs/lsp-mode-setup-completion)))

;; Use dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

(use-package corfu-doc
  ;; :hook (corfu-mode . corfu-doc-mode)
  :config
  (define-key corfu-map (kbd "M-d") #'corfu-doc-toggle)
  (define-key corfu-map (kbd "C-M-p") #'corfu-doc-scroll-down)
  (define-key corfu-map (kbd "C-M-n") #'corfu-doc-scroll-up))

(use-package cape-yasnippet
  :quelpa (cape-yasnippet :fetcher github
    	                  :repo "elken/cape-yasnippet"
    	                  :files ("*.el")))

(use-package cape
  :preface
  (defun petmacs/set-lsp-capfs ()
	(setq-local completion-at-point-functions
				(list #'lsp-completion-at-point
					  #'cape-yasnippet
					  #'cape-file
					  #'cape-dabbrev)))
  ;; :bind (("C-M-o" . cape-file))
  :hook (lsp-completion-mode . petmacs/set-lsp-capfs)
  :init (setq cape-dabbrev-min-length 2
              cape-dabbrev-check-other-buffers nil)
  :config
  ;; 默认用这三个补全后端
  ;; (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(unless (display-graphic-p)
  (use-package corfu-terminal
    :quelpa (corfu-terminal :fetcher git
  		                    :url "https://codeberg.org/akib/emacs-corfu-terminal.git"
  		                    :files ("*.el"))
    :init (corfu-terminal-mode +1))

  (use-package corfu-doc-terminal
    :quelpa (corfu-doc-terminal :fetcher git
  		                        :url "https://codeberg.org/akib/emacs-corfu-doc-terminal.git"
  		                        :files ("*.el"))
    :init (corfu-doc-terminal-mode +1)))


(provide 'init-corfu)
