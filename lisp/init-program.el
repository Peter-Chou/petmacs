;; init-program.el --- Setup programming useful packages.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package fill-column-indicator
  :preface
  (defun petmacs//change-fci-rule-color (&rest args)
    "Change the fill-column-indicator based on the background.
    ARGS is only used because we use this function as advice after
    `load-theme'."
    (setq fci-rule-color (face-attribute 'font-lock-function-name-face :foreground))
    (let* ((bufs (buffer-list)))
      (dolist (buf bufs)
	(with-current-buffer buf
          (when (bound-and-true-p fci-mode)
            (fci-make-overlay-strings)
            (fci-update-all-windows t))))))
  :hook (prog-mode . (lambda ()
		       (fci-mode 1)
		       (fci-update-all-windows t)))
  :init
  (setq fci-rule-color (face-attribute 'font-lock-function-name-face :foreground))
  (setq fci-rule-use-dashes t)
  (advice-add 'load-theme :after 'petmacs//change-fci-rule-color))

(use-package imenu-list
  :defer t
  :hook (imenu-list-major-mode . (lambda ()
				   (display-line-numbers-mode -1)
				   (hl-line-mode -1)))
  :init
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize nil)
  :init
  (evil-define-key 'normal imenu-list-major-mode-map (kbd "d") 'imenu-list-display-entry)
  (evil-define-key 'normal imenu-list-major-mode-map (kbd "r") 'imenu-list-refresh) 
  (evil-define-key 'normal imenu-list-major-mode-map (kbd "q") 'imenu-list-quit-window)
  (evil-define-key 'normal imenu-list-major-mode-map [down-mouse-1] 'imenu-list-display-entry))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package prettify-utils
  :quelpa
  (prettify-utils :repo "Ilazki/prettify-utils.el" :fetcher github))

(use-package pretty-code
  :ensure nil
  :init
  (require 'pretty-code)
  (pretty-code-add-hook 'python-mode-hook     '((:def "def")
    					        (:lambda "lambda")))
  (pretty-code-add-hook 'emacs-lisp-mode-hook '((:def "defun")
						(:lambda "lambda"))))

(use-package pretty-fonts
:ensure nil
:defer nil
:hook (after-make-frame-functions . petmacs/complete-setup-pretty-code)
:preface
(defun petmacs/complete-setup-pretty-code ()
  (require 'pretty-fonts)
  (pretty-fonts-add-hook 'prog-mode-hook pretty-fonts-fira-code-alist)
  ;; (pretty-fonts-add-hook 'org-mode-hook  pretty-fonts-fira-code-alist)

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

(use-package electric-operator
  :hook ((c-mode-common . electric-operator-mode)
         (python-mode . electric-operator-mode)
         (electric-operator-mode . (lambda ()
                                     (electric-operator-add-rules-for-mode 'c++-mode
                                                                           (cons "*" nil)
                                                                           (cons "&" nil))
                                     (electric-operator-add-rules-for-mode 'c-mode
                                                                           (cons "*" nil))))))

;; using outline-minor-mode for evil folding
(use-package outline-mode
  :ensure nil
  :hook ((prog-mode . outline-minor-mode)
	 (python-mode . petmacs//python-mode-outline-hook)
	 (emacs-lisp-mode . petmacs//el-mode-outline-hook)
	 (sh-mode . petmacs//sh-mode-outline-hook))
  ;; (add-hook 'python-mode-hook 'petmacs//python-mode-outline-hook)
  ;; (add-hook 'emacs-lisp-mode-hook 'petmacs//el-mode-outline-hook)
  ;; (add-hook 'sh-mode-hook 'petmacs//sh-mode-outline-hook)
  :preface
  ;; get from https://gist.github.com/alphapapa/79ea7c33d03c9c975634559b1a776418
  (defun petmacs//python-mode-outline-hook ()
    (setq outline-level 'petmacs//python-outline-level)

    (setq outline-regexp
	  (rx (or
	       ;; Commented outline heading
	       (group
		(* space)	 ; 0 or more spaces
		(one-or-more (syntax comment-start))
		(one-or-more space)
		;; Heading level
		(group (repeat 1 8 "\*"))	 ; Outline stars
		(one-or-more space))

	       ;; Python keyword heading
	       (group
		;; Heading level

		;; TODO: Try setting this to python-indent-offset
		;; instead of space.  Might capture the indention levels
		;; better.
		(group (* space))	; 0 or more spaces
		bow
		;; Keywords
		(or "class" "def")
		eow)))))

  (defun petmacs//python-outline-level ()
    ;; Based on this code found at
    ;; http://blog.zenspider.com/blog/2013/07/my-emacs-setup-ruby-and-outline.html:
    ;; (or (and (match-string 1)
    ;;	     (or (cdr (assoc (match-string 1) outline-heading-alist))
    ;;		 (- (match-end 1) (match-beginning 1))))
    ;;	(and (match-string 0)
    ;;	     (cdr (assoc (match-string 0) outline-heading-alist)))

    ;; This doesn't work properly. It sort-of works, but it's not
    ;; correct. Running this function consecutively on the same line
    ;; sometimes returns different results. And it doesn't seem to
    ;; correctly recognize top-level Python functions or classes as
    ;; top-level headings, so subheadings beneath them don't collapse
    ;; properly.

    (or
     ;; Commented outline heading
     (and (string-match (rx
			 (* space)
			 (one-or-more (syntax comment-start))
			 (one-or-more space)
			 (group (one-or-more "\*"))
			 (one-or-more space))
			(match-string 0))
	  (- (match-end 0) (match-beginning 0)))

     ;; Python keyword heading, set by number of indentions
     ;; Add 8 (the highest standard outline level) to every Python keyword heading
     (+ 8 (- (match-end 0) (match-beginning 0)))))

  (defun petmacs//sh-outline-level ()
    (or
     ;; Commented outline heading
     (and (string-match (rx
			 (* space)
			 (one-or-more (syntax comment-start))
			 (one-or-more space)
			 (group (one-or-more "\*"))
			 (one-or-more space))
			(match-string 0))
	  (- (match-end 1) (match-beginning 1) 1))

     ;; Keyword/function heading
     ;; Add 8 (the highest standard outline level) to every keyword
     ;; heading
     (+ 8 (- (match-end 3) (match-beginning 3)))))

  (defun petmacs//sh-mode-outline-hook ()
    (setq outline-level 'petmacs//sh-outline-level)
    (setq outline-regexp (rx (group (or
				     ;; Outline headings
				     (and (* space)
					  (one-or-more (syntax comment-start))
					  (* space)
					  (group (one-or-more "\*"))
					  (* space))

				     ;; Keywords and functions
				     (and (group (* space))
					  (or
					   ;; e.g. "function foo"
					   (and (or "function" "if" "elif" "else" "for" "while")
						(one-or-more space))

					   ;; e.g. "foo()"
					   (and (one-or-more (or alnum "_-"))
						(* space)
						(syntax open-parenthesis)
						(syntax close-parenthesis)))))))))

  (defun petmacs//el-outline-level ()
    (or
     ;; Commented outline heading
     (and (string-match (rx
			 (* space)
			 (group (one-or-more (syntax comment-start)))
			 (one-or-more space))
			(match-string 0))
	  (- (match-end 0) (match-beginning 0) 1))

     ;; Lisp def heading
     ;; Add 8 (the highest standard outline level) to every keyword
     ;; heading
     (+ 8 (- (match-end 0) (match-beginning 0)))))

  (defun petmacs//el-mode-outline-hook ()
    (setq outline-level 'petmacs//el-outline-level)
    (setq outline-regexp "\\(;;[;]\\{1,8\\} \\|\\((defun\\)\\)"))

  (defun petmacs//general-outline-level ()
    (or
     ;; Commented outline heading
     (and (string-match (rx
			 (* space)
			 (one-or-more (syntax comment-start))
			 (one-or-more space)
			 (group (one-or-more "\*"))
			 (one-or-more space))
			(match-string 0))
	  (- (match-end 1) (match-beginning 1) 1))))

  (defun petmacs//general-outline-mode-enable ()
    (interactive)
    (setq outline-level 'petmacs//general-outline-level)
    (setq outline-regexp (rx (group (* space)
                                    (one-or-more (syntax comment-start))
                                    (* space)
                                    (group (one-or-more "\*"))
                                    (* space))))
    (outline-minor-mode))
  :init
  ;; (evil-define-key 'normal outline-mode-map (kbd "zK") 'outline-show-branches) ; Show all children recursively but no body.
  ;; (evil-define-key 'normal outline-mode-map (kbd "zk") 'outline-show-children) ; Direct children only unlike `outline-show-branches'
  (define-key evil-normal-state-map (kbd "zB") 'outline-hide-body) ; Hide all bodies
  (define-key evil-normal-state-map (kbd "zb") 'outline-show-all)  ; Hide current body
  (define-key evil-normal-state-map (kbd "ze") 'outline-show-entry) ; Show current body only, not subtree, reverse of outline-hide-entry
  (define-key evil-normal-state-map (kbd "zl") 'outline-hide-leaves) ; Like `outline-hide-body' but for current subtree only
  (define-key evil-normal-state-map (kbd "zp") 'outline-hide-other)    ; Hide all nodes and bodies except current body.
  (define-key evil-normal-state-map (kbd "zj") 'outline-forward-same-level)
  (define-key evil-normal-state-map (kbd "zk") 'outline-backward-same-level)
  (define-key evil-normal-state-map (kbd "M-j") 'outline-move-subtree-down)
  (define-key evil-normal-state-map (kbd "M-k") 'outline-move-subtree-up))

;; Compilation Mode
(use-package compile
  :ensure nil
  :preface
  ;; ANSI Coloring
  ;; @see https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
  (defun my-colorize-compilation-buffer ()
    "ANSI coloring in compilation buffers."
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . my-colorize-compilation-buffer))

(use-package dockerfile-mode)

(provide 'init-program)

;;; init-program.el ends here
