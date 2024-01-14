;; -*- lexical-binding: t no-byte-compile: t -*-

(require 'init-const)
(require 'init-custom)

(defun petmacs/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (save-excursion
    (if (and (= 1 (length (window-list)))
             (assoc ?_ register-alist))
        (jump-to-register ?_)
      (progn
        (window-configuration-to-register ?_)
        (delete-other-windows)))))

(defun petmacs/split-window-vertically-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun petmacs/split-window-horizontally-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun petmacs//confirm-kill-buffer ()
  "Prompt the user to save a buffer to a file before killing it.
This skips the following buffers:
- A buffer with non-nil value of variable `buffer-file-name'.
  Or in other words, a buffer who has a file associated with.
  Emacs by default prompts the user to save it if it's modified.
- A buffer derived from `special-mode'."
  (when (and (not buffer-file-name)
             (buffer-modified-p)
             (not (derived-mode-p 'special-mode))
             (not (yes-or-no-p (format "Buffer %S modified; kill anyway? " (buffer-name)))))
    (save-buffer)))

(defun petmacs/new-empty-buffer (&optional split)
  "Create a new buffer called: \"untitled\".

SPLIT decides where the buffer opens:
- nil, open in current window.
- `left', `below', `above' or `right', split the window in the given direction.
- `frame', open in new frame.

If the variable `petmacs-new-empty-buffer-major-mode' has been set,
then apply that major mode to the new buffer."
  (interactive)
  (let ((newbuf (generate-new-buffer "untitled")))
    (cl-case split
      (left  (split-window-horizontally))
      (below (petmacs/split-window-vertically-and-switch))
      (above (split-window-vertically))
      (right (petmacs/split-window-horizontally-and-switch))
      (frame (select-frame (make-frame))))
    ;; Prompt to save on `save-some-buffers' with positive PRED
    (with-current-buffer newbuf
      (setq-local buffer-offer-save t)
      (add-hook 'kill-buffer-hook
                #'petmacs//confirm-kill-buffer
                nil t)
      (when petmacs-new-empty-buffer-major-mode
        (funcall petmacs-new-empty-buffer-major-mode)))
    ;; pass non-nil force-same-window to prevent `switch-to-buffer' from
    ;; displaying buffer in another window
    (switch-to-buffer newbuf nil 'force-same-window)))

(defun petmacs/new-empty-buffer-new-frame ()
  "Create a new buffer called untitled(<n>),
in a new frame."
  (interactive)
  (petmacs/new-empty-buffer 'frame))

(defun petmacs/new-empty-buffer-left ()
  "Create a new buffer called untitled(<n>),
in a split window to the left."
  (interactive)
  (petmacs/new-empty-buffer 'left))

(defun petmacs/new-empty-buffer-below ()
  "Create a new buffer called untitled(<n>),
in a split window below."
  (interactive)
  (petmacs/new-empty-buffer 'below))

(defun petmacs/new-empty-buffer-above ()
  "Create a new buffer called untitled(<n>),
in a split window above."
  (interactive)
  (petmacs/new-empty-buffer 'above))

(defun petmacs/new-empty-buffer-right ()
  "Create a new buffer called untitled(<n>),
in a split window to the right."
  (interactive)
  (petmacs/new-empty-buffer 'right))

(defun petmacs/kill-other-buffers (&optional arg)
  "Kill all other buffers.
If the universal prefix argument is used then kill the windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
                             (buffer-name)))
    (let* ((buffers-to-kill (if (bound-and-true-p persp-mode)
                                (persp-buffer-list)
                              (buffer-list))))
      (mapc 'kill-buffer (delq (current-buffer) buffers-to-kill)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "Buffers deleted!")))

(defun petmacs/kill-this-buffer (&optional arg)
  "Kill the current buffer.
If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (equal '(4) arg)
        (kill-buffer-and-window)
      (kill-buffer))))

(defun petmacs/save-as (filename &optional visit)
  "Save current buffer or active region as specified file.
When called interactively, it first prompts for FILENAME, and then asks
whether to VISIT it, and if so, whether to show it in current window or
another window. When prefixed with a universal-argument \\[universal-argument], include
filename in prompt.

FILENAME  a non-empty string as the name of the saved file.
VISIT     When it's `:current', open FILENAME in current window. When it's
          `:other', open FILENAME in another window. When it's nil, only
          save to FILENAME but does not visit it. (Default to `:current'
          when called from a LISP program.)

When FILENAME already exists, it also asks the user whether to
overwrite it."
  (interactive (let* ((filename (expand-file-name (read-file-name "Save buffer as: " nil nil nil
                                                                  (when current-prefix-arg (buffer-name)))))
                      (choices  '("Current window"
                                  "Other window"
                                  "Don't open"))
                      (actions  '(:current :other nil))
                      (visit    (let ((completion-ignore-case t))
                                  (nth (cl-position
                                        (completing-read "Do you want to open the file? "
                                                         choices nil t)
                                        choices
                                        :test #'equal)
                                       actions))))
                 (list filename visit)))
  (unless (called-interactively-p 'any)
    (cl-assert (and (stringp filename)
                    (not (string-empty-p filename))
                    (not (directory-name-p filename)))
               t "Expect a non-empty filepath, found: %s")
    (setq filename (expand-file-name filename)
          visit (or visit :other))
    (let ((choices '(:current :other nil)))
      (cl-assert (memq visit choices)
                 t "Found %s, expect one of %s")))
  (let ((dir (file-name-directory filename)))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (if (use-region-p)
      (write-region (region-beginning) (region-end) filename nil nil nil t)
    (write-region nil nil filename nil nil nil t))
  (pcase visit
    (:current (find-file filename))
    (:other   (funcall-interactively 'find-file-other-window filename))))

(defun petmacs/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (if (yes-or-no-p
           (format "Are you sure you want to delete this file: '%s'?" name))
          (progn
            (delete-file filename t)
            (kill-buffer buffer)
            (when (and (configuration-layer/package-used-p 'projectile)
                       (projectile-project-p))
              (call-interactively #'projectile-invalidate-cache))
            (message "File deleted: '%s'" filename))
        (message "Canceled: File deletion")))))

(defun petmacs/sudo-edit (&optional arg)
  (interactive "P")
  (require 'tramp)
  (let ((fname (if (or arg (not buffer-file-name))
                   (read-file-name "File: ")
                 buffer-file-name)))
    (find-file
     (if (not (tramp-tramp-file-p fname))
         (concat "/sudo:root@localhost:" fname)
       (with-parsed-tramp-file-name fname parsed
                                    (when (equal parsed-user "root")
                                      (error "Already root!"))
                                    (let* ((new-hop (tramp-make-tramp-file-name
                                                     ;; Try to retrieve a tramp method suitable for
                                                     ;; multi-hopping
                                                     (cond ((tramp-get-method-parameter
                                                             parsed 'tramp-login-program))
                                                           ((tramp-get-method-parameter
                                                             parsed 'tramp-copy-program))
                                                           (t parsed-method))
                                                     parsed-user
                                                     parsed-domain
                                                     parsed-host
                                                     parsed-port
                                                     nil
                                                     parsed-hop))
                                           (new-hop (substring new-hop 1 -1))
                                           (new-hop (concat new-hop "|"))
                                           (new-fname (tramp-make-tramp-file-name
                                                       "sudo"
                                                       parsed-user
                                                       parsed-domain
                                                       parsed-host
                                                       parsed-port
                                                       parsed-localname
                                                       new-hop)))
                                      new-fname))))))

(defun petmacs/delete-window (&optional arg)
  "Delete the current window.
If the universal prefix argument is used then kill the buffer too."
  (interactive "P")
  (if (equal '(4) arg)
      (kill-buffer-and-window)
    (delete-window)))

(defun petmacs/switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(defun petmacs/evil-goto-definition-other-window ()
  "Jump to definition around point in other window."
  (interactive)
  (let ((pos (point)))
    (switch-to-buffer-other-window (current-buffer))
    (goto-char pos)
    (evil-goto-definition)))

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

(defun petmacs/find-user-early-init-file ()
  "Edit the `early-init-file', in the current window."
  (interactive)
  (find-file-existing early-init-file))

(defun petmacs/find-dotfile ()
  "Edit the `dotfile', in the current window."
  (interactive)
  (find-file-existing (concat user-emacs-directory "init.el")))

(defun petmacs/pyenv-executable-find (command)
  "Find executable taking pyenv shims into account.
If the executable is a system executable and not in the same path
as the pyenv version then also return nil. This works around https://github.com/pyenv/pyenv-which-ext
"
  (if (and (not (and (boundp 'pyvenv-virtual-env) pyvenv-virtual-env)) (executable-find "pyenv"))
      (progn
        (let ((pyenv-string (shell-command-to-string (concat "pyenv which " command)))
              (pyenv-version-names (split-string (string-trim (shell-command-to-string "pyenv version-name")) ":"))
              (executable nil)
              (i 0))
          (if (not (string-match "not found" pyenv-string))
              (while (and (not executable)
                          (< i (length pyenv-version-names)))
                (if (string-match (elt pyenv-version-names i) (string-trim pyenv-string))
                    (setq executable (string-trim pyenv-string)))
                (if (string-match (elt pyenv-version-names i) "system")
                    (setq executable (string-trim (executable-find command))))
                (setq i (1+ i))))
          executable))
    (executable-find command)))

(defun petmacs/python-execute-file (arg)
  "Execute a python script in a shell."
  (interactive "P")
  ;; set compile command to buffer-file-name
  ;; universal argument put compile buffer in comint mode
  (let ((universal-argument t)
        (compile-command (format "%s %s"
                                 (petmacs/pyenv-executable-find python-shell-interpreter)
                                 (shell-quote-argument (file-name-nondirectory buffer-file-name)))))
    (if arg
        (call-interactively 'compile)
      (compile compile-command t)
      (with-current-buffer (get-buffer "*compilation*")
        (inferior-python-mode)))))

(defun petmacs/python-execute-file-focus (arg)
  "Execute a python script in a shell and switch to the shell buffer in
 `insert state'."
  (interactive "P")
  (petmacs/python-execute-file arg)
  (switch-to-buffer-other-window "*compilation*")
  (end-of-buffer)
  (evil-insert-state))

(defun petmacs/python-remove-unused-imports ()
  "Use Autoflake to remove unused function"
  "autoflake --remove-all-unused-imports -i unused_imports.py"
  (interactive)
  (if (executable-find "autoflake")
      (progn
        (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                               (shell-quote-argument (buffer-file-name))))
        (revert-buffer t t t))
    (message "Error: Cannot find autoflake executable.")))

(defun petmacs/quit-subjob ()
  "quit runing job in python buffer"
  (interactive)
  (save-excursion
    (setq petmacs--current-buffer-name (buffer-name))
    (previous-buffer)

    (setq petmacs--previous-buffer-name (buffer-name))
    (switch-to-buffer "*compilation*")
    (comint-quit-subjob)
    (switch-to-buffer petmacs--previous-buffer-name)
    (switch-to-buffer petmacs--current-buffer-name)))

(defun petmacs/rename-current-buffer-file (&optional arg)
  "Rename the current buffer and the file it is visiting.
If the buffer isn't visiting a file, ask if it should
be saved to a file, or just renamed.
If called without a prefix argument, the prompt is
initialized with the current filename."
  (interactive "P")
  (let* ((name (buffer-name))
         (filename (buffer-file-name)))
    (if (and filename (file-exists-p filename))
        ;; the buffer is visiting a file
        (let* ((dir (file-name-directory filename))
               (new-name (read-file-name "New name: " (if arg dir filename))))
          (cond ((get-buffer new-name)
                 (error "A buffer named '%s' already exists!" new-name))
                (t
                 (let ((dir (file-name-directory new-name)))
                   (when (and (not (file-exists-p dir))
                              (yes-or-no-p
                               (format "Create directory '%s'?" dir)))
                     (make-directory dir t)))
                 (rename-file filename new-name 1)
                 (rename-buffer new-name)
                 (set-visited-file-name new-name)
                 (set-buffer-modified-p nil)
                 (when (fboundp 'recentf-add-file)
                   (recentf-add-file new-name)
                   (recentf-remove-if-non-kept filename))
                 (when (projectile-project-p)
                   (call-interactively #'projectile-invalidate-cache))
                 (message "File '%s' successfully renamed to '%s'"
                          name (file-name-nondirectory new-name)))))
      ;; the buffer is not visiting a file
      (let ((key))
        (while (not (memq key '(?s ?r)))
          (setq key (read-key (propertize

                               (format
                                (concat "Buffer '%s' is not visiting a file: "
                                        "[s]ave to file or [r]ename buffer?")
                                name)
                               'face 'minibuffer-prompt)))
          (cond ((eq key ?s)            ; save to file
                 ;; this allows for saving a new empty (unmodified) buffer
                 (unless (buffer-modified-p) (set-buffer-modified-p t))
                 (save-buffer))
                ((eq key ?r)            ; rename buffer
                 (let ((new-name (read-string "New buffer name: ")))
                   (while (get-buffer new-name)
                     ;; ask to rename again, if the new buffer name exists
                     (if (yes-or-no-p
                          (format (concat "A buffer named '%s' already exists: "
                                          "Rename again?")
                                  new-name))
                         (setq new-name (read-string "New buffer name: "))
                       (keyboard-quit)))
                   (rename-buffer new-name)
                   (message "Buffer '%s' successfully renamed to '%s'"
                            name new-name)))
                ;; ?\a = C-g, ?\e = Esc and C-[
                ((memq key '(?\a ?\e)) (keyboard-quit))))))))

(defun petmacs/copy-file ()
  "Write the file under new name."
  (interactive)
  (call-interactively 'write-file))

(defun petmacs//vterm-other-window ()
  "Open a `shell' in a new window."
  (interactive)
  (select-frame (make-frame))
  (toggle-frame-maximized)
  (vterm)
  (evil-insert-state))

(defun petmacs/toggle-flycheck-error-list ()
  "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
  (interactive)
  (-if-let (window (flycheck-get-error-list-window))
      (quit-window nil window)
    (flycheck-list-errors)))

(defun petmacs/next-error (&optional n reset)
  "Dispatch to flycheck or standard emacs error."
  (interactive "P")
  (let ((sys (petmacs/error-delegate)))
    (cond
     ((eq 'flycheck sys) (call-interactively 'flycheck-next-error))
     ((eq 'emacs sys) (call-interactively 'next-error)))))

(defun petmacs/previous-error (&optional n reset)
  "Dispatch to flycheck or standard emacs error."
  (interactive "P")
  (let ((sys (petmacs/error-delegate)))
    (cond
     ((eq 'flycheck sys) (call-interactively 'flycheck-previous-error))
     ((eq 'emacs sys) (call-interactively 'previous-error)))))

(defun petmacs/shell-pop ()
  "Open a term buffer at projectile project root."
  (interactive)
  (call-interactively 'shell-pop)
  (evil-insert-state))

(defun petmacs/projectile-shell-pop ()
  "Open a term buffer at projectile project root."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (call-interactively 'shell-pop)
    (evil-insert-state)))

(defun petmacs/treemacs-project-toggle ()
  "Toggle and add the current project to treemacs if not already added."
  (interactive)
  (if (eq (treemacs-current-visibility) 'visible)
      (delete-window (treemacs-get-local-window))
    (let ((path (projectile-ensure-project (projectile-project-root)))
          (name (projectile-project-name)))
      (unless (treemacs-current-workspace)
        (treemacs--find-workspace))
      (treemacs-do-add-project-to-workspace path name)
      (treemacs-select-window))))

(defun petmacs/consult-theme ()
  (interactive)
  (call-interactively #'consult-theme)
  (if (featurep 'awesome-tray)
      (awesome-tray-enable)))

(defun json-to-single-line (beg end)
  "Collapse prettified json in region between BEG and END to a single line"
  (interactive "r")
  (if (use-region-p)
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (goto-char (point-min))
          (while (re-search-forward "[[:space:]\n]+" nil t)
            (replace-match " "))))
    (print "This function operates on a region")))

(defun petmacs/run-ts-file ()
  "Compile and run the current TypeScript file."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (base-name (file-name-sans-extension file-name))
         (js-file-name (concat base-name ".js")))
    (progn
      (message "Compiling %s to %s..." file-name js-file-name)
      (call-process "tsc" nil nil nil file-name))
    (message "Running %s..." js-file-name)
    (async-shell-command (concat "node " js-file-name))))

(defun petmacs--projectile-file-path ()
  "Retrieve the file path relative to project root.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not visit a file."
  (when-let (file-name (buffer-file-name))
    (file-relative-name (file-truename file-name) (projectile-project-root))))

(defun petmacs--projectile-file-path-with-line ()
  "Retrieve the file path relative to project root, including line number.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not visit a file."
  (when-let (file-path (petmacs--projectile-file-path))
    (concat file-path ":" (number-to-string (line-number-at-pos)))))


(defun petmacs--projectile-file-path-with-line-column ()
  "Retrieve the file path relative to project root, including line and column number.

This function respects the the `column-number-indicator-zero-based' value.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not visit a file."
  (when-let (file-path (petmacs--projectile-file-path-with-line))
    (format "%s:%s" file-path
            (+ (current-column) (if column-number-indicator-zero-based 0 1)))))

(defun petmacs/projectile-copy-file-path-with-line-column ()
  "Copy and show the file path relative to project root, including line and column number.

This function respects the value of the `column-number-indicator-zero-based'
variable."
  (interactive)
  (if-let (file-path (petmacs--projectile-file-path-with-line-column))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not visiting a file!")))

(defun petmacs--projectile-directory-path ()
  "Retrieve the directory path relative to project root.

If the buffer is not visiting a file, use the `list-buffers-directory'
variable as a fallback to display the directory, useful in buffers like the
ones created by `magit' and `dired'.

Returns:
  - A string containing the directory path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (directory-name (if-let (file-name (buffer-file-name))
                                (file-name-directory file-name)
                              list-buffers-directory))
    (file-relative-name
     (file-truename directory-name)
     (projectile-project-root))))

(defun petmacs/projectile-copy-directory-path ()
  "Copy and show the directory path relative to project root.

If the buffer is not visiting a file, use the `list-buffers-directory'
variable as a fallback to display the directory, useful in buffers like the
ones created by `magit' and `dired'."
  (interactive)
  (if-let (directory-path (petmacs--projectile-directory-path))
      (progn
        (kill-new directory-path)
        (message "%s" directory-path))
    (message "WARNING: Current buffer does not have a directory!")))

(defun petmacs/projectile-copy-file-path ()
  "Copy and show the file path relative to project root."
  (interactive)
  (if-let (file-path (petmacs--projectile-file-path))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not visiting a file!")))

(defun petmacs--file-path ()
  "Retrieve the file path of the current buffer.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (buffer-file-name))
    (file-truename file-path)))

(defun petmacs/copy-file-path ()
  "Copy and show the file path of the current buffer."
  (interactive)
  (if-let (file-path (petmacs--file-path))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun petmacs--directory-path ()
  "Retrieve the directory path of the current buffer.

If the buffer is not visiting a file, use the `list-buffers-directory' variable
as a fallback to display the directory, useful in buffers like the ones created
by `magit' and `dired'.

Returns:
  - A string containing the directory path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (directory-name (if-let (file-name (buffer-file-name))
                                (file-name-directory file-name)
                              list-buffers-directory))
    (file-truename directory-name)))

(defun petmacs/copy-directory-path ()
  "Copy and show the directory path of the current buffer.

If the buffer is not visiting a file, use the `list-buffers-directory'
variable as a fallback to display the directory, useful in buffers like the
ones created by `magit' and `dired'."
  (interactive)
  (if-let (directory-path (petmacs--directory-path))
      (progn
        (kill-new directory-path)
        (message "%s" directory-path))
    (message "WARNING: Current buffer does not have a directory!")))

(defun petmacs/copy-file-name ()
  "Copy and show the file name of the current buffer."
  (interactive)
  (if-let* ((file-path (petmacs--file-path))
            (file-name (file-name-nondirectory file-path)))
      (progn
        (kill-new file-name)
        (message "%s" file-name))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun petmacs/copy-file-name-base ()
  "Copy and show the file name without its final extension of the current
buffer."
  (interactive)
  (if-let (file-name (file-name-base (petmacs--file-path)))
      (progn
        (kill-new file-name)
        (message "%s" file-name))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun petmacs/copy-buffer-name ()
  "Copy and show the name of the current buffer."
  (interactive)
  (kill-new (buffer-name))
  (message "%s" (buffer-name)))

(defun petmacs/dos2unix ()
  "Converts the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun petmacs/unix2dos ()
  "Converts the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

(defun split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

(defun petmacs/toggle-treemacs-and-symbols-outline ()
  (interactive)
  (let* ((buf (current-buffer)))
    (petmacs/treemacs-project-toggle)
    (switch-to-buffer buf)
    (petmacs/imenu-list-smart-toggle)
    (switch-to-buffer buf)))

(defun remove-dos-eol ()
  "Remove  in current region or buffer."
  (interactive)
  (save-excursion
    (when (region-active-p)
      (narrow-to-region (region-beginning) (region-end)))
    (goto-char (point-min))
    (let ((count 0))
      (while (search-forward "" nil t)
        (replace-match "" nil t)
        (setq count (1+ count)))
      (message "Removed %d " count))
    (widen)))

(provide 'core-funcs)
