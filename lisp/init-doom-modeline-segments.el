;; init-doom-modeline-segments.el --- doom modeline custom segments	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; doom-modeline Custom Segments
;;

;;; Code:

(require 'doom-modeline-segments)

(doom-modeline-def-segment my-nyan-position
  "The buffer position information."
  (let ((sep (doom-modeline-spc))
        (wsep (doom-modeline-wspc))
        (face (doom-modeline-face))
        (help-echo "Buffer percentage\n\
mouse-1: Display Line and Column Mode Menu")
        (mouse-face 'doom-modeline-highlight)
        (local-map mode-line-column-line-number-mode-map))
    `(,wsep

      ;; Line and column
      (:propertize
       ((line-number-mode
         (column-number-mode
          (doom-modeline-column-zero-based
           doom-modeline-position-column-line-format
           ,(string-replace
             "%c" "%C" (car doom-modeline-position-column-line-format)))
          doom-modeline-position-line-format)
         (column-number-mode
          (doom-modeline-column-zero-based
           doom-modeline-position-column-format
           ,(string-replace
             "%c" "%C" (car doom-modeline-position-column-format)))))
        (doom-modeline-total-line-number
         ,(and doom-modeline-total-line-number
               (format "/%d" (line-number-at-pos (point-max))))))
       face ,face
       help-echo ,help-echo
       mouse-face ,mouse-face
       local-map ,local-map)

      ((or line-number-mode column-number-mode)
       ,sep)

      ;; must show nyan
      ,(cond
        ((bound-and-true-p nyan-mode)
         (concat sep (nyan-create) sep))
        ((bound-and-true-p poke-line-mode)
         (concat sep (poke-line-create) sep))
        ((bound-and-true-p mlscroll-mode)
         (concat sep
                 (let ((mlscroll-right-align nil))
                   (format-mode-line (mlscroll-mode-line)))
                 sep))
        ((bound-and-true-p sml-modeline-mode)
         (concat sep (sml-modeline-create) sep))
        (t ""))

      ;; Percent position
      (doom-modeline-percent-position
       ((:propertize ("" doom-modeline-percent-position)
         face ,face
         help-echo ,help-echo
         mouse-face ,mouse-face
         local-map ,local-map)
        ,sep)))))

(doom-modeline-def-segment my-breadcrumb
  "breadcrumb mode in modeline"
  (if (and (doom-modeline--active) (> (length (breadcrumb-imenu-crumbs)) 0))
      `(,(propertize
          (format " %s %s "
                  (nerd-icons-codicon "nf-cod-triangle_right")
                  (nerd-icons-codicon "nf-cod-symbol_method"))
          'face `(:inherit font-lock-function-name-face :height 1.2)) ,(breadcrumb-imenu-crumbs) " ")
    '("")))

(doom-modeline-def-segment my-which-function-segment
  "breadcrumb mode in modeline"
  `(,(propertize
      (format " %s %s " (nerd-icons-codicon "nf-cod-triangle_right")
              (which-function))
      'face `(:inherit font-lock-function-name-face))))

(provide 'init-doom-modeline-segments)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-doom-modeline-segments.el ends here
