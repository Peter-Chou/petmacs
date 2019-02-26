;; init-highlight.el --- Setup highlights.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Highlight the current line
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

;; Highlight symbols
(use-package symbol-overlay
  :diminish
  :functions (symbol-overlay-switch-first symbol-overlay-switch-last)
  :commands (symbol-overlay-get-symbol
             symbol-overlay-assoc
             symbol-overlay-get-list
             symbol-overlay-jump-call)
  :hook ((prog-mode . symbol-overlay-mode))
  :config
  (defun symbol-overlay-switch-first ()
    (interactive)
    (let* ((symbol (symbol-overlay-get-symbol))
           (keyword (symbol-overlay-assoc symbol))
           (a-symbol (car keyword))
           (before (symbol-overlay-get-list a-symbol 'car))
           (count (length before)))
      (symbol-overlay-jump-call 'symbol-overlay-basic-jump (- count))))

  (defun symbol-overlay-switch-last ()
    (interactive)
    (let* ((symbol (symbol-overlay-get-symbol))
           (keyword (symbol-overlay-assoc symbol))
           (a-symbol (car keyword))
           (after (symbol-overlay-get-list a-symbol 'cdr))
           (count (length after)))
      (symbol-overlay-jump-call 'symbol-overlay-basic-jump (- count 1))))

  (bind-keys :map symbol-overlay-map
             ("<" . symbol-overlay-switch-first)
             (">" . symbol-overlay-switch-last))
  (global-set-key (kbd "M-i") 'symbol-overlay-put)
  (global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
  (global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
  (global-set-key (kbd "<f7>") 'symbol-overlay-mode)
  (global-set-key (kbd "M-c") 'symbol-overlay-remove-all)
  ;; (global-set-key (kbd "<f8>") 'symbol-overlay-remove-all)

  ;; remap help tooltip keybinding from h to H in symbol-overlay-map
  (define-key symbol-overlay-map (kbd "h") 'evil-backward-char)
  (define-key symbol-overlay-map (kbd "H") 'symbol-overlay-map-help))

(use-package highlight-indent-guides
  :defer t
  :hook ((python-mode . highlight-indent-guides-mode)
         ;; (prog-mode . highlight-indent-guides-mode)
         (highlight-indent-guides-mode . (lambda ()
                                           (set-face-foreground 'highlight-indent-guides-character-face "#8f9091")
                                           (set-face-foreground 'highlight-indent-guides-top-character-face "#fe5e10"))))
  :config
  (progn
    (setq highlight-indent-guides-method 'character
          
          highlight-indent-guides-character ?\┆ ;; candidates: , ⋮, ┆, ┊, ┋, ┇
          highlight-indent-guides-responsive 'top
          highlight-indent-guides-auto-enabled nil
          highlight-indent-guides-auto-character-face-perc 10
          highlight-indent-guides-auto-top-character-face-perc 20)))

;; Highlight matching paren
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t))

;; Highlight TODO and similar keywords in comments and strings
(use-package hl-todo
  :custom-face (hl-todo ((t (:box t :inherit 'hl-todo))))
  :bind (:map hl-todo-mode-map
              ([C-f3] . hl-todo-occur)
              ("C-c t p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("C-c t o" . hl-todo-occur))
  :hook (after-init . global-hl-todo-mode)
  :config
  (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
  (dolist (keyword '("WORKAROUND" "HACK" "TRICK"))
    (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces)))
(provide 'init-highlight)

;;; init-highlight.el ends here
