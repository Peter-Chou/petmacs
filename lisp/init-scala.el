;; -*- lexical-binding: t no-byte-compile: t -*-

(use-package scala-mode
  :preface
  (defun petmacs/scala-join-line ()
    "Adapt `scala-indent:join-line' to behave more like evil's line join.
`scala-indent:join-line' acts like the vanilla `join-line',
joining the current line with the previous one. The vimmy way is
to join the current line with the next.
Try to move to the subsequent line and then join. Then manually move
point to the position of the join."
    (interactive)
    (let (join-pos)
      (save-excursion
	    (goto-char (line-end-position))
	    (unless (eobp)
          (forward-line)
          (call-interactively 'scala-indent:join-line)
          (setq join-pos (point))))

      (when join-pos
	    (goto-char join-pos))))
  ;;   (defun petmacs//scala-display-sbt-at-bottom (buffer args)
  ;;     "Display a short buffer in a dedicated window at frame bottom.
  ;; For use with `sbt:display-buffer-action'."
  ;;     (set-window-dedicated-p
  ;;      (display-buffer-at-bottom buffer (cons '(window-height . 12) args))
  ;;      t))
  :interpreter ("scala" . scala-mode)
  :init
  (progn
    (dolist (ext '(".cfe" ".cfs" ".si" ".gen" ".lock"))
      (add-to-list 'completion-ignored-extensions ext)))
  :config
  (progn
    ;; Automatically insert asterisk in a comment when enabled
    (defun scala/newline-and-indent-with-asterisk ()
      (interactive)
      (newline-and-indent)
      (when scala-auto-insert-asterisk-in-comments
        (scala-indent:insert-asterisk-on-multiline-comment)))

    (evil-define-key 'insert scala-mode-map
      (kbd "RET") 'scala/newline-and-indent-with-asterisk)

    (evil-define-key 'normal scala-mode-map "J" 'petmacs/scala-join-line)

    ;; (when (eq scala-sbt-window-position 'bottom)
    ;;   (setq sbt:display-buffer-action
    ;;         (list #'petmacs//scala-display-sbt-at-bottom)))

    ;; Compatibility with `aggressive-indent'
    (setq scala-indent:align-forms t
          scala-indent:align-parameters t
          scala-indent:default-run-on-strategy
          scala-indent:operator-strategy)))

(provide 'init-scala)
