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

(use-package imenu-list
  :defer t
  :hook (imenu-list-major-mode . (lambda ()
				   (display-line-numbers-mode -1)
				   (hl-line-mode -1)
				   (vim-empty-lines-mode -1)))
  :init
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize t)
  :init
  (evil-define-key 'normal imenu-list-major-mode-map (kbd "d") 'imenu-list-display-entry) 
  (evil-define-key 'normal imenu-list-major-mode-map (kbd "r") 'imenu-list-refresh) 
  (evil-define-key 'normal imenu-list-major-mode-map (kbd "q") 'imenu-list-quit-window))


;; (use-package prettify-utils) ; needed by pretty-code

(use-package prettify-utils
  :ensure t
  :quelpa
  (prettify-utils :repo "Ilazki/prettify-utils.el" :fetcher github))

(use-package pretty-code
  :ensure nil
  :defer nil
  :init
  (progn
    (require 'pretty-code)
    (pretty-code-add-hook 'emacs-lisp-mode-hook '((:def "defun")))
    (pretty-code-add-hook 'python-mode-hook     '((:def "def")
					          (:lambda "lambda")))))

(use-package pretty-fonts
  :ensure nil
  :defer nil
  :hook (after-make-frame-functions . petmacs/complete-setup-pretty-code)
  :preface
  (defun petmacs/complete-setup-pretty-code ()
    (require 'pretty-fonts)
    (pretty-fonts-add-hook 'prog-mode-hook pretty-fonts-fira-code-alist)
    (pretty-fonts-add-hook 'org-mode-hook  pretty-fonts-fira-code-alist)

    (pretty-fonts-set-fontsets-for-fira-code)
    (pretty-fonts-set-fontsets
     '(;; All-the-icons fontsets
       ("fontawesome"
	;;                         
	#xf07c #xf0c9 #xf0c4 #xf0cb #xf017 #xf101)

       ("all-the-icons"
	;;    
	#xe907 #xe928)

       ("github-octicons"
	;;                               
	#xf091 #xf059 #xf076 #xf075 #xe192  #xf016 #xf071)

       ("material icons"
	;;              
	#xe871 #xe918 #xe3e7  #xe5da
	;;              
	#xe3d0 #xe3d1 #xe3d2 #xe3d4)))
    )
  :init
  (petmacs/complete-setup-pretty-code))

(provide 'init-program)
