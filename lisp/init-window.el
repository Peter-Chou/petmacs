;; init-window.el --- Setup window related packages.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package ace-window
  :custom
  (aw-keys '(?j ?k ?l ?i ?o ?h ?y ?u ?p))
  :custom-face
  (aw-leading-char-face ((t (:height 4.0 :foreground "#f1fa8c"))))
  :init
  (progn
    (setq aw-scope 'frame)
    (global-set-key (kbd "C-x O") 'other-frame)
    (global-set-key [remap other-window] 'ace-window)))

(use-package winum
  :init
  (winum-mode)
  :config
  (define-key winum-keymap (kbd "M-1") 'winum-select-window-1)
  (define-key winum-keymap (kbd "M-2") 'winum-select-window-2)
  (define-key winum-keymap (kbd "M-3") 'winum-select-window-3)
  (define-key winum-keymap (kbd "M-4") 'winum-select-window-4)
  (define-key winum-keymap (kbd "M-5") 'winum-select-window-5)
  (define-key winum-keymap (kbd "M-6") 'winum-select-window-6)
  (define-key winum-keymap (kbd "M-7") 'winum-select-window-7)
  (define-key winum-keymap (kbd "M-8") 'winum-select-window-8))

;; (use-package popwin
;;   :hook (after-init . popwin-mode)
;;   :config
;;   (progn
;;     ;; (require 'popwin)
;;     ;; (popwin-mode 1)

;;     ;; don't use default value but manage it ourselves
;;     (setq popwin:special-display-config nil)

;;     ;; buffers that we manage
;;     (push '("*Help*"                 :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
;;     (push '("*Process List*"         :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
;;     (push '("*compilation*"          :dedicated t :position bottom :stick t :noselect t   :height 0.5) popwin:special-display-config)
;;     (push '("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
;;     (push '("*Async Shell Command*"  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
;;     (push '(" *undo-tree*"           :dedicated t :position right  :stick t :noselect nil :width   60) popwin:special-display-config)
;;     (push '("*undo-tree Diff*"       :dedicated t :position bottom :stick t :noselect nil :height 0.3) popwin:special-display-config)
;;     (push '("*ert*"                  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
;;     (push '("*grep*"                 :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
;;     (push '("*nosetests*"            :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
;;     (push '("^\*WoMan.+\*$" :regexp t             :position bottom                                   ) popwin:special-display-config)
;;     (push '("*Google Translate*"     :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
;;     (push '("^\\*Flycheck.+\\*$" :regexp t :dedicated t :position bottom :stick t :noselect t) popwin:special-display-config)
;;     ))

;; Enforce rules for popups
(defvar shackle--popup-window-list nil) ; all popup windows
(defvar-local shackle--current-popup-window nil) ; current popup window
(put 'shackle--current-popup-window 'permanent-local t)

(use-package shackle
  :preface
  (defun petmacs/shackle-popup-message-buffer ()
    "View message buffer."
    (interactive)
    (ignore-errors
      (display-buffer "*Messages*")))

  (defun petmacs/shackle-popup-compilation-buffer ()
    "View compilation buffer."
    (interactive)
    (ignore-errors
      (display-buffer"*compilation*")))

  :commands shackle-display-buffer
  :hook (after-init . shackle-mode)
  :config
(eval-and-compile
  (defun petamcs/shackle-popup-last-buffer ()
    "View last popup buffer."
    (interactive)
    (ignore-errors
      (display-buffer shackle-last-buffer)))

  ;; Add keyword: `autoclose'
  (defun shackle-display-buffer-hack (fn buffer alist plist)
    (let ((window (funcall fn buffer alist plist)))
      (setq shackle--current-popup-window window)

      (when (plist-get plist :autoclose)
        (push (cons window buffer) shackle--popup-window-list))
      window))

  (defun shackle-close-popup-window-hack (&rest _)
    "Close current popup window via `C-g'."
    (setq shackle--popup-window-list
          (cl-loop for (window . buffer) in shackle--popup-window-list
                   if (and (window-live-p window)
                           (equal (window-buffer window) buffer))
                   collect (cons window buffer)))
    ;; `C-g' can deactivate region
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p)))
      (let (window buffer)
        (if (one-window-p)
            (progn
              (setq window (selected-window))
              (when (equal (buffer-local-value 'shackle--current-popup-window
                                               (window-buffer window))
                           window)
                (winner-undo)))
          (setq window (caar shackle--popup-window-list))
          (setq buffer (cdar shackle--popup-window-list))
          (when (and (window-live-p window)
                     (equal (window-buffer window) buffer))
            (delete-window window)

            (pop shackle--popup-window-list))))))

  (advice-add #'keyboard-quit :before #'shackle-close-popup-window-hack)
  (advice-add #'shackle-display-buffer :around #'shackle-display-buffer-hack))

  ;; rules
  (setq shackle-default-size 0.4)
  (setq shackle-default-alignment 'below)
  (setq shackle-default-rule nil)
  (setq shackle-rules
        '(("*Help*" :select t :size 0.3 :align 'below :autoclose t)
          ("*compilation*" :size 0.3 :align 'below :autoclose t)
          ("*Completions*" :size 0.3 :align 'below :autoclose t)
          ("*Pp Eval Output*" :size 15 :align 'below :autoclose t)
          ("*ert*" :align 'below :autoclose t)
          ("*Backtrace*" :select t :size 15 :align 'below)
          ("*Warnings*" :size 0.3 :align 'below :autoclose t)
          ("*Messages*" :size 0.3 :align 'below :autoclose t)
          ("^\\*.*Shell Command.*\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
          ("\\*[Wo]*Man.*\\*" :regexp t :select t :align 'below :autoclose t)
          ("*Calendar*" :select t :size 0.3 :align 'below)
          (" *undo-tree*" :select t)
          ("*Paradox Report*" :size 0.3 :align 'below :autoclose t)
          ("*quickrun*" :select t :size 15 :align 'below)
          ("*tldr*" :align 'below :autoclose t)
          ("*Youdao Dictionary*" :size 0.3 :align 'below :autoclose t)
          ("*Finder*" :select t :size 0.3 :align 'below :autoclose t)
          ("^\\*elfeed-entry" :regexp t :size 0.7 :align 'below :autoclose t)
          ("^\\*lsp-help\\*$" :regexp t :size 0.4 :align 'below :autoclose t)

          (ag-mode :select t :align 'below)
          (grep-mode :select t :align 'below)
          (ivy-occur-grep-mode :select t :align 'below)
          (pt-mode :select t :align 'below)
          (rg-mode :select t :align 'below)

          (flycheck-error-list-mode :select t :size 0.3 :align 'below :autoclose t)
          (flymake-diagnostics-buffer-mode :select t :size 0.3 :align 'below :autoclose t)

          (Buffer-menu-mode :select t :size 20 :align 'below :autoclose t)
          (comint-mode :align 'below)
          (helpful-mode :select t :size 0.4 :align 'below :autoclose t)
          (process-menu-mode :select t :size 0.3 :align 'below :autoclose t)
          (list-environment-mode :select t :size 0.3 :align 'below :autoclose t)
          (profiler-report-mode :select t :size 0.5 :align 'below)
          (tabulated-list-mode :align 'below))))


;; center window
(use-package olivetti
  :diminish
  :bind ("<f7>" . olivetti-mode)
  :init (setq olivetti-body-width 0.56))
;; (use-package writeroom-mode
;;   :init
;;   (setq writeroom-maximize-window nil
;; 	writeroom-fullscreen-effect 'maximized
;; 	writeroom-mode-line t
;; 	writeroom-width 100)
;;   :config
;;   (define-key writeroom-mode-map (kbd "C-M-<") #'writeroom-decrease-width)
;;   (define-key writeroom-mode-map (kbd "C-M->") #'writeroom-increase-width)
;;   (define-key writeroom-mode-map (kbd "C-M-=") #'writeroom-adjust-width))


(provide 'init-window)

;;; init-window.el ends here
