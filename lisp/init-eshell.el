;; init-eshell.el --- Setup eshell.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package eshell
  :ensure nil
  :defines (compilation-last-buffer eshell-prompt-function)
  :commands (eshell/alias
             eshell-send-input eshell-flatten-list
             eshell-interactive-output-p eshell-parse-command)
  :hook ((eshell-mode . (lambda ()
                          (bind-key "C-l" 'petmacs/eshell-clear eshell-mode-map)
			  (evil-define-key 'normal eshell-mode-map (kbd "C-r") 'petmacs/ivy-eshell-history)
			  (evil-define-key 'insert eshell-mode-map (kbd "C-r") 'petmacs/ivy-eshell-history)))
	 (eshell-mode  . (lambda () (display-line-numbers-mode -1)(hl-line-mode -1)))
	 (eshell-after-prompt . petmacs//protect-eshell-prompt))
  :preface
  (defun petmacs/eshell-clear ()
    "Clear the eshell buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

  (defun petmacs/ivy-eshell-history ()
    (interactive)
    (require 'em-hist)
    (let* ((start-pos (save-excursion (eshell-bol) (point)))
           (end-pos (point))
           (input (buffer-substring-no-properties start-pos end-pos))
           (command (ivy-read "Command: "
                              (delete-dups
                               (when (> (ring-size eshell-history-ring) 0)
				 (ring-elements eshell-history-ring)))
                              :initial-input input)))
      (setf (buffer-substring start-pos end-pos) command)
      (end-of-line)))

  (defun petmacs//protect-eshell-prompt ()
    "Protect Eshell's prompt like Comint's prompts.

E.g. `evil-change-whole-line' won't wipe the prompt. This
is achieved by adding the relevant text properties."
    (let ((inhibit-field-text-motion t))
      (add-text-properties
       (point-at-bol)
       (point)
       '(rear-nonsticky t
			inhibit-line-move-field-capture t
			field output
			read-only t
			front-sticky (field inhibit-line-move-field-capture)))))
  :init
  (setq eshell-history-size 512)
  ;; add alias to eshell
  (setq eshell-aliases-file (expand-file-name "alias" user-emacs-directory)))

(use-package eshell-prompt-extras
  :custom-face
  (epe-pipeline-delimiter-face ((t (:foreground "#fd780f" :weight bold))))
  (epe-pipeline-host-face ((t (:foreground "#3cd8a2" :weight bold))))
  (epe-pipeline-time-face ((t (:foreground "#e2c504"))))
  (epe-pipeline-user-face ((t (:foreground "#ef2d2d" :weight bold))))
  :init
  (with-eval-after-load "esh-opt"
    ;; (require 'virtualenvwrapper)
    ;; (venv-initialize-eshell)
    (autoload 'epe-theme-pipeline "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
          ;; add new line adhead of tty
          eshell-prompt-function (lambda ()
                                   (concat "\n" (epe-theme-pipeline))))))

;; Fish-like history autosuggestions
;; disable because of lagging
;; (use-package esh-autosuggest
;;   :pin melpa-stable
;;   :defines ivy-display-functions-alist
;;   :preface
;;   (defun setup-eshell-ivy-completion ()
;;     (setq-local ivy-display-functions-alist
;;                 (remq (assoc 'ivy-completion-in-region ivy-display-functions-alist)
;; 		      ivy-display-functions-alist)))
;;   :bind (:map eshell-mode-map
;; 	      ([remap eshell-pcomplete] . completion-at-point))
;;   :hook ((eshell-mode . esh-autosuggest-mode)
;;          (eshell-mode . setup-eshell-ivy-completion)))

;; Eldoc support
(use-package esh-help
  :init (setup-esh-help-eldoc))

;; `cd' to frequent directory in eshell
(use-package eshell-z
  :hook (eshell-mode . (lambda () (require 'eshell-z))))

(provide 'init-eshell)

;;; init-eshell.el ends here
