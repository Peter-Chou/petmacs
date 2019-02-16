
(defun petmacs/find-dotfile ()
  "Edit the `dotfile', in the current window."
  (interactive)
  (find-file-existing (concat user-emacs-directory "init.el")))


(defun petmacs/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window)))
    ;; if no window is found in the windows history, `switch-to-buffer' will
    ;; default to calling `other-buffer'.
    (switch-to-buffer
     (cl-find-if (lambda (buffer)
                   (not (eq buffer current-buffer)))
                 (mapcar #'car (window-prev-buffers window)))
     nil t)))

(provide 'my-funcs)
