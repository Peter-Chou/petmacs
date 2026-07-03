;; init-hydra.el --- Initialize hydra configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Tool configurations.
;;

;;; Code:

(use-package hydra
  :defines (consult-imenu-config posframe-border-width)
  :functions childframe-completion-workable-p
  :hook ((emacs-lisp-mode . hydra-add-imenu)
         ((after-init after-load-theme server-after-make-frame) . hydra-set-posframe))
  :init
  (with-eval-after-load 'consult-imenu
    (setq consult-imenu-config
          '((emacs-lisp-mode :toplevel "Functions"
                             :types ((?f "Functions" font-lock-function-name-face)
                                     (?h "Hydras"    font-lock-constant-face)
                                     (?m "Macros"    font-lock-function-name-face)
                                     (?p "Packages"  font-lock-constant-face)
                                     (?t "Types"     font-lock-type-face)
                                     (?v "Variables" font-lock-variable-name-face))))))

  (defun hydra-set-posframe ()
    "Set display type and appearance of hydra."
    ;; Display type
    (if (childframe-completion-workable-p)
        (setq hydra-hint-display-type 'posframe)
      (setq hydra-hint-display-type 'lv))
    ;; Appearance
    (setq hydra-posframe-show-params
          `(:left-fringe 8
            :right-fringe 8
            :internal-border-width ,posframe-border-width
            :internal-border-color ,(face-background 'posframe-border nil t)
            :background-color ,(face-background 'tooltip nil t)
            :foreground-color ,(face-foreground 'tooltip nil t)
            :lines-truncate t
            :poshandler posframe-poshandler-frame-center-near-bottom))))

(use-package pretty-hydra
  :functions icons-displayable-p
  :hook (emacs-lisp-mode . pretty-hydra-add-imenu)
  :init
  (require 'pretty-hydra)
  (defun pretty-hydra-add-imenu ()
    "Have hydras in `imenu'."
    (add-to-list 'imenu-generic-expression
                 '("Hydras" "^.*(\\(pretty-hydra-define\\) \\([a-zA-Z-]+\\)" 2)))

  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face 'mode-line-emphasis))
          (height (or height 1.2))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (icons-displayable-p) icon-type icon-name)
         (let ((f (intern (format "nerd-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face)))))

(provide 'init-hydra)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-hydra.el ends here
