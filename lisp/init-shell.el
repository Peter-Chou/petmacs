;; init-shell.el --- Setup shell.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(use-package shell
  :ensure nil
  :hook ((shell-mode . my-shell-mode-hook)
         (comint-output-filter-functions . comint-strip-ctrl-m))
  :init
  (setq system-uses-terminfo nil)

  (with-no-warnings
    (defun my-shell-simple-send (proc command)
      "Various PROC COMMANDs pre-processing before sending to shell."
      (cond
       ;; Checking for clear command and execute it.
       ((string-match "^[ \t]*clear[ \t]*$" command)
        (comint-send-string proc "\n")
        (erase-buffer))
       ;; Checking for man command and execute it.
       ((string-match "^[ \t]*man[ \t]*" command)
        (comint-send-string proc "\n")
        (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
        (setq command (replace-regexp-in-string "[ \t]+$" "" command))
        ;;(message (format "command %s command" command))
        (funcall 'man command))
       ;; Send other commands to the default handler.
       (t (comint-simple-send proc command))))

    (defun my-shell-mode-hook ()
      "Shell mode customization."
      (local-set-key '[up] 'comint-previous-input)
      (local-set-key '[down] 'comint-next-input)
      (local-set-key '[(shift tab)] 'comint-next-matching-input-from-input)

      (ansi-color-for-comint-mode-on)
      (setq comint-input-sender 'my-shell-simple-send))))

;; ANSI & XTERM 256 color support
(use-package xterm-color
  :defines (compilation-environment
            eshell-preoutput-filter-functions
            eshell-output-filter-functions)
  :functions (compilation-filter my-advice-compilation-filter)
  :init
  ;; For shell and interpreters
  (setenv "TERM" "xterm-256color")
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
  (add-hook 'shell-mode-hook
            (lambda ()
              ;; Disable font-locking to improve performance
              (font-lock-mode -1)
              ;; Prevent font-locking from being re-enabled
              (make-local-variable 'font-lock-function)
              (setq font-lock-function #'ignore)))

  ;; For eshell
  (with-eval-after-load 'esh-mode
    (add-hook 'eshell-before-prompt-hook
              (lambda ()
                (setq xterm-color-preserve-properties t)))
    (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
    (setq eshell-output-filter-functions
          (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

  ;; For compilation buffers
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun my-advice-compilation-filter (f proc string)
    (funcall f proc
             (if (eq major-mode 'rg-mode) ; compatible with `rg'
                 string
               (xterm-color-filter string))))
  (advice-add 'compilation-filter :around #'my-advice-compilation-filter)
  (advice-add 'gud-filter :around #'my-advice-compilation-filter))

;; Better term
;; @see https://github.com/akermu/emacs-libvterm#installation
;; (when (and module-file-suffix           ; dynamic module
;;            (executable-find "cmake")
;;            (executable-find "libtool")
;;            (executable-find "make"))
;;   (use-package vterm
;;     :bind (:map vterm-mode-map
;;            ([f9] . shell-pop))
;;     :init (setq vterm-kill-buffer-on-exit t
;; 		vterm-always-compile-module t)
;;     :hook (vterm-mode . (lambda ()
;; 			  ;; (setq-local evil-insert-state-cursor 'box)
;; 			  (evil-insert-state)))
;;     :config
;;     (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)))

;; Better term
;; @see https://github.com/akermu/emacs-libvterm#installation
(when (and module-file-suffix           ; dynamic module
           (executable-find "cmake")
           (executable-find "libtool")
           (executable-find "make"))
  (use-package vterm
    :commands vterm--internal
    :bind (:map vterm-mode-map
           ([f9] . (lambda ()
                     (interactive)
                     (and (fboundp 'shell-pop)
                          (shell-pop nil)))))
    :init
    (setq vterm-always-compile-module t)

    (with-no-warnings
      (when (childframe-workable-p)
        (defvar vterm-posframe--frame nil)

        (defun vterm-posframe-hidehandler (_)
          "Hidehandler used by `vterm-posframe-toggle'."
          (not (eq (selected-frame) posframe--frame)))

        (defun vterm-posframe-toggle ()
          "Toggle `vterm' child frame."
          (interactive)
          (let ((buffer (vterm--internal #'ignore 100)))
            (if (and vterm-posframe--frame
                     (frame-live-p vterm-posframe--frame)
                     (frame-visible-p vterm-posframe--frame))
                (progn
                  (posframe-hide buffer)
                  ;; Focus the parent frame
                  (select-frame-set-input-focus (frame-parent vterm-posframe--frame)))
              (let ((width  (max 80 (/ (frame-width) 2)))
                    (height (/ (frame-height) 2)))
                (setq vterm-posframe--frame
                      (posframe-show
                       buffer
                       :poshandler #'posframe-poshandler-frame-center
                       :hidehandler #'vterm-posframe-hidehandler
                       :left-fringe 8
                       :right-fringe 8
                       :width width
                       :height height
                       :min-width width
                       :min-height height
                       :internal-border-width 3
                       :internal-border-color (face-foreground 'font-lock-comment-face nil t)
                       :background-color (face-background 'tooltip nil t)
                       :override-parameters '((cursor-type . t))
                       :accept-focus t))
                ;; Blink cursor
                (with-current-buffer buffer
                  (save-excursion
                    (vterm-clear))
                  (setq-local cursor-type 'box))
                ;; Focus the child frame
                (select-frame-set-input-focus vterm-posframe--frame)))))
        (bind-key "C-`" #'vterm-posframe-toggle)))
    :config
    (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
    ))

;; Shell Pop
;; Shell Pop
(use-package shell-pop
  :bind (("C-`" . (lambda ()
                    (interactive)
                    (if (fboundp 'vterm-posframe-toggle)
                        (vterm-posframe-toggle)
                      (shell-pop nil))))
         ([f9] . shell-pop))
  :init
  (setq shell-pop-window-size 30
        shell-pop-shell-type
        (cond ((fboundp 'vterm) '("vterm" "*vterm*" #'vterm))
              (sys/win32p '("eshell" "*eshell*" #'eshell))
              (t '("terminal" "*terminal*"
                   (lambda () (term shell-pop-term-shell)))))))

;; (use-package eshell
;;   :ensure nil
;;   :defines (compilation-last-buffer eshell-prompt-function)
;;   :commands (eshell/alias
;; 	     eshell-send-input eshell-flatten-list
;; 	     eshell-interactive-output-p eshell-parse-command)
;;   :hook ((eshell-mode . (lambda ()
;;                           (bind-key "C-l" 'petmacs/eshell-clear eshell-mode-map)
;; 			  (evil-define-key 'normal eshell-mode-map (kbd "C-r") 'petmacs/ivy-eshell-history)
;; 			  (evil-define-key 'insert eshell-mode-map (kbd "C-r") 'petmacs/ivy-eshell-history)))
;; 	 (eshell-mode  . (lambda () (display-line-numbers-mode -1)(hl-line-mode -1)))
;; 	 (eshell-after-prompt . petmacs//protect-eshell-prompt))
;;   :preface
;;   (defun petmacs/eshell-clear ()
;;     "Clear the eshell buffer."
;;     (interactive)
;;     (let ((inhibit-read-only t))
;;       (erase-buffer)
;;       (eshell-send-input)))

;;   (defun petmacs/ivy-eshell-history ()
;;     (interactive)
;;     (require 'em-hist)
;;     (let* ((start-pos (save-excursion (eshell-bol) (point)))
;; 	   (end-pos (point))
;; 	   (input (buffer-substring-no-properties start-pos end-pos))
;; 	   (command (ivy-read "Command: "
;; 			      (delete-dups
;; 			       (when (> (ring-size eshell-history-ring) 0)
;; 				 (ring-elements eshell-history-ring)))
;; 			      :initial-input input)))
;;       (setf (buffer-substring start-pos end-pos) command)
;;       (end-of-line)))

;;   (defun petmacs//protect-eshell-prompt ()
;;     "Protect Eshell's prompt like Comint's prompts.

;; E.g. `evil-change-whole-line' won't wipe the prompt. This
;; is achieved by adding the relevant text properties."
;;     (let ((inhibit-field-text-motion t))
;;       (add-text-properties
;;        (point-at-bol)
;;        (point)
;;        '(rear-nonsticky t
;; 			inhibit-line-move-field-capture t
;; 			field output
;; 			read-only t
;; 			front-sticky (field inhibit-line-move-field-capture)))))
;;   :init
;;   (setq eshell-history-size 512)
;;   ;; add alias to eshell
;;   (setq eshell-aliases-file (expand-file-name "alias" user-emacs-directory)))

;; (use-package eshell-prompt-extras
;;   :custom-face
;;   (epe-pipeline-delimiter-face ((t (:foreground "#fd780f" :weight bold))))
;;   (epe-pipeline-host-face ((t (:foreground "#3cd8a2" :weight bold))))
;;   (epe-pipeline-time-face ((t (:foreground "#e2c504"))))
;;   (epe-pipeline-user-face ((t (:foreground "#ef2d2d" :weight bold))))
;;   :init
;;   (with-eval-after-load "esh-opt"
;;     ;; (require 'virtualenvwrapper)
;;     ;; (venv-initialize-eshell)
;;     (autoload 'epe-theme-pipeline "eshell-prompt-extras")
;;     (setq eshell-highlight-prompt nil
;;           ;; add new line adhead of tty
;;           eshell-prompt-function (lambda ()
;; 				   (concat "\n" (epe-theme-pipeline))))))

;; ;; ANSI & XTERM 256 color support
;; (use-package xterm-color
;;   :defines (compilation-environment
;; 	    eshell-preoutput-filter-functions
;; 	    eshell-output-filter-functions)
;;   :functions (compilation-filter my-advice-compilation-filter)
;;   :init
;;   ;; For shell and interpreters
;;   (setenv "TERM" "xterm-256color")
;;   (setq comint-output-filter-functions
;;         (remove 'ansi-color-process-output comint-output-filter-functions))
;;   (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
;;   (add-hook 'shell-mode-hook
;; 	    (lambda ()
;; 	      ;; Disable font-locking to improve performance
;; 	      (font-lock-mode -1)
;; 	      ;; Prevent font-locking from being re-enabled
;; 	      (make-local-variable 'font-lock-function)
;; 	      (setq font-lock-function #'ignore)))

;;   ;; For eshell
;;   (with-eval-after-load 'esh-mode
;;     (add-hook 'eshell-before-prompt-hook
;; 	      (lambda ()
;;                 (setq xterm-color-preserve-properties t)))
;;     (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
;;     (setq eshell-output-filter-functions
;;           (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

;;   ;; For compilation buffers
;;   (setq compilation-environment '("TERM=xterm-256color"))
;;   (defun my-advice-compilation-filter (f proc string)
;;     (funcall f proc
;; 	     (if (eq major-mode 'rg-mode) ; compatible with `rg'
;;                  string
;; 	       (xterm-color-filter string))))
;;   (advice-add 'compilation-filter :around #'my-advice-compilation-filter)
;;   (advice-add 'gud-filter :around #'my-advice-compilation-filter))

;; ;; Eldoc support
;; (use-package esh-help
;;   :init (setup-esh-help-eldoc))

;; ;; `cd' to frequent directory in eshell
;; (use-package eshell-z
;;   :hook (eshell-mode . (lambda () (require 'eshell-z))))

;; ;; (if sys/linuxp
;; ;;     ;; sudo apt-get install libtool libtool-bin cmake
;; ;;     (progn
;; ;;       (use-package vterm
;; ;; 	:preface
;; ;; 	(defun evil-collection-vterm-escape-stay ()
;; ;; 	  "Go back to normal state but don't move
;; ;; cursor backwards. Moving cursor backwards is the default vim behavior but it is
;; ;; not appropriate in some cases like terminals."
;; ;; 	  (setq-local evil-move-cursor-back nil))
;; ;; 	:init
;; ;; 	(setq vterm-kill-buffer-on-exit t
;; ;; 	      vterm-always-compile-module t)
;; ;; 	:config
;; ;; 	(add-hook 'vterm-mode-hook #'evil-collection-vterm-escape-stay)
;; ;; 	(add-hook 'vterm-mode-hook
;; ;; 		  (lambda ()
;; ;; 		    ;; (setq-local evil-insert-state-cursor 'box)
;; ;; 		    (evil-insert-state)))
;; ;; 	(define-key vterm-mode-map [return]                      #'vterm-send-return)

;; ;; 	(setq vterm-keymap-exceptions nil)
;; ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
;; ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
;; ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
;; ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
;; ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
;; ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
;; ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
;; ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
;; ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
;; ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
;; ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
;; ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
;; ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
;; ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
;; ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
;; ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
;; ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
;; ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
;; ;; 	(evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
;; ;; 	(evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
;; ;; 	(evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
;; ;; 	(evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
;; ;; 	(evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
;; ;; 	(evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
;; ;; 	(evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume)
;; ;; 	(evil-define-key 'normal vterm-mode-map (kbd "P") 'vterm-yank)
;; ;; 	(evil-define-key 'normal vterm-mode-map (kbd "p") 'vterm-yank)
;; ;; 	)

;; ;;       (use-package multi-vterm)
;; ;;       (use-package vterm-toggle
;; ;; 	:preface
;; ;; 	(defun petmacs/projectile-pop-vterm ()
;; ;; 	  "Open a term buffer at projectile project root."
;; ;; 	  (interactive)
;; ;; 	  (let ((default-directory (projectile-project-root)))
;; ;; 	    (call-interactively 'vterm-toggle))))
;; ;;       )
;; ;;   (
;; ;;    (use-package eshell
;; ;;      :ensure nil
;; ;;      :defines (compilation-last-buffer eshell-prompt-function)
;; ;;      :commands (eshell/alias
;; ;; 		eshell-send-input eshell-flatten-list
;; ;; 		eshell-interactive-output-p eshell-parse-command)
;; ;;      :hook ((eshell-mode . (lambda ()
;; ;;                              (bind-key "C-l" 'petmacs/eshell-clear eshell-mode-map)
;; ;; 			     (evil-define-key 'normal eshell-mode-map (kbd "C-r") 'petmacs/ivy-eshell-history)
;; ;; 			     (evil-define-key 'insert eshell-mode-map (kbd "C-r") 'petmacs/ivy-eshell-history)))
;; ;; 	    (eshell-mode  . (lambda () (display-line-numbers-mode -1)(hl-line-mode -1)))
;; ;; 	    (eshell-after-prompt . petmacs//protect-eshell-prompt))
;; ;;      :preface
;; ;;      (defun petmacs/eshell-clear ()
;; ;;        "Clear the eshell buffer."
;; ;;        (interactive)
;; ;;        (let ((inhibit-read-only t))
;; ;; 	 (erase-buffer)
;; ;; 	 (eshell-send-input)))

;; ;;      (defun petmacs/ivy-eshell-history ()
;; ;;        (interactive)
;; ;;        (require 'em-hist)
;; ;;        (let* ((start-pos (save-excursion (eshell-bol) (point)))
;; ;; 	      (end-pos (point))
;; ;; 	      (input (buffer-substring-no-properties start-pos end-pos))
;; ;; 	      (command (ivy-read "Command: "
;; ;; 				 (delete-dups
;; ;; 				  (when (> (ring-size eshell-history-ring) 0)
;; ;; 				    (ring-elements eshell-history-ring)))
;; ;; 				 :initial-input input)))
;; ;; 	 (setf (buffer-substring start-pos end-pos) command)
;; ;; 	 (end-of-line)))

;; ;;      (defun petmacs//protect-eshell-prompt ()
;; ;;        "Protect Eshell's prompt like Comint's prompts.

;; ;; E.g. `evil-change-whole-line' won't wipe the prompt. This
;; ;; is achieved by adding the relevant text properties."
;; ;;        (let ((inhibit-field-text-motion t))
;; ;; 	 (add-text-properties
;; ;; 	  (point-at-bol)
;; ;; 	  (point)
;; ;; 	  '(rear-nonsticky t
;; ;; 			   inhibit-line-move-field-capture t
;; ;; 			   field output
;; ;; 			   read-only t
;; ;; 			   front-sticky (field inhibit-line-move-field-capture)))))
;; ;;      :init
;; ;;      (setq eshell-history-size 512)
;; ;;      ;; add alias to eshell
;; ;;      (setq eshell-aliases-file (expand-file-name "alias" user-emacs-directory)))

;; ;;    (use-package eshell-prompt-extras
;; ;;      :custom-face
;; ;;      (epe-pipeline-delimiter-face ((t (:foreground "#fd780f" :weight bold))))
;; ;;      (epe-pipeline-host-face ((t (:foreground "#3cd8a2" :weight bold))))
;; ;;      (epe-pipeline-time-face ((t (:foreground "#e2c504"))))
;; ;;      (epe-pipeline-user-face ((t (:foreground "#ef2d2d" :weight bold))))
;; ;;      :init
;; ;;      (with-eval-after-load "esh-opt"
;; ;;        ;; (require 'virtualenvwrapper)
;; ;;        ;; (venv-initialize-eshell)
;; ;;        (autoload 'epe-theme-pipeline "eshell-prompt-extras")
;; ;;        (setq eshell-highlight-prompt nil
;; ;;              ;; add new line adhead of tty
;; ;;              eshell-prompt-function (lambda ()
;; ;; 				      (concat "\n" (epe-theme-pipeline))))))

;; ;;    ;; ANSI & XTERM 256 color support
;; ;;    (use-package xterm-color
;; ;;      :defines (compilation-environment
;; ;; 	       eshell-preoutput-filter-functions
;; ;; 	       eshell-output-filter-functions)
;; ;;      :functions (compilation-filter my-advice-compilation-filter)
;; ;;      :init
;; ;;      ;; For shell and interpreters
;; ;;      (setenv "TERM" "xterm-256color")
;; ;;      (setq comint-output-filter-functions
;; ;;            (remove 'ansi-color-process-output comint-output-filter-functions))
;; ;;      (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
;; ;;      (add-hook 'shell-mode-hook
;; ;; 	       (lambda ()
;; ;; 		 ;; Disable font-locking to improve performance
;; ;; 		 (font-lock-mode -1)
;; ;; 		 ;; Prevent font-locking from being re-enabled
;; ;; 		 (make-local-variable 'font-lock-function)
;; ;; 		 (setq font-lock-function #'ignore)))

;; ;;      ;; For eshell
;; ;;      (with-eval-after-load 'esh-mode
;; ;;        (add-hook 'eshell-before-prompt-hook
;; ;; 		 (lambda ()
;; ;;                    (setq xterm-color-preserve-properties t)))
;; ;;        (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
;; ;;        (setq eshell-output-filter-functions
;; ;;              (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

;; ;;      ;; For compilation buffers
;; ;;      (setq compilation-environment '("TERM=xterm-256color"))
;; ;;      (defun my-advice-compilation-filter (f proc string)
;; ;;        (funcall f proc
;; ;; 		(if (eq major-mode 'rg-mode) ; compatible with `rg'
;; ;;                     string
;; ;; 		  (xterm-color-filter string))))
;; ;;      (advice-add 'compilation-filter :around #'my-advice-compilation-filter)
;; ;;      (advice-add 'gud-filter :around #'my-advice-compilation-filter))

;; ;;    ;; Fish-like history autosuggestions
;; ;;    ;; disable because of lagging
;; ;;    ;; (use-package esh-autosuggest
;; ;;    ;;   :pin melpa-stable
;; ;;    ;;   :defines ivy-display-functions-alist
;; ;;    ;;   :preface
;; ;;    ;;   (defun setup-eshell-ivy-completion ()
;; ;;    ;;     (setq-local ivy-display-functions-alist
;; ;;    ;;                 (remq (assoc 'ivy-completion-in-region ivy-display-functions-alist)
;; ;;    ;; 		      ivy-display-functions-alist)))
;; ;;    ;;   :bind (:map eshell-mode-map
;; ;;    ;; 	      ([remap eshell-pcomplete] . completion-at-point))
;; ;;    ;;   :hook ((eshell-mode . esh-autosuggest-mode)
;; ;;    ;;          (eshell-mode . setup-eshell-ivy-completion)))

;; ;;    ;; Eldoc support
;; ;;    (use-package esh-help
;; ;;      :init (setup-esh-help-eldoc))

;; ;;    ;; `cd' to frequent directory in eshell
;; ;;    (use-package eshell-z
;; ;;      :hook (eshell-mode . (lambda () (require 'eshell-z))))
;; ;;    )
;; ;;   )


(provide 'init-shell)

;;; init-shell.el ends here
