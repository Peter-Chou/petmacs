;; core-funcs.el --- Petmacs usable functions.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(defvar socks-noproxy)
(defvar socks-server)

(defun petmacs/open-gnome-terminal ()
  (interactive)
  (shell-command "gnome-terminal"))

;; Suppress warnings
(declare-function async-inject-variables 'async)
(declare-function chart-bar-quickie 'chart)
(declare-function flycheck-buffer 'flycheck)
(declare-function flymake-start 'flymake)
(declare-function upgrade-packages 'init-package)

(defun petmacs//setup-default-key-name (key desc)
  (which-key-add-key-based-replacements
    (format "%s %s" petmacs-evil-leader-key key) desc)
  (which-key-add-key-based-replacements
    (format "%s %s" petmacs-evil-major-leader-insert-default-key key) desc))

(defun petmacs//setup-major-mode-key-name (key name)
  (which-key-add-key-based-replacements (format "%s m%s" petmacs-evil-leader-key key) name)
  (which-key-add-key-based-replacements (format "%s m%s" petmacs-evil-major-leader-insert-default-key key) name)
  (which-key-add-key-based-replacements (format ", %s" key) name))

(defun petmacs//vterm-other-window ()
  "Open a `shell' in a new window."
  (interactive)
  (select-frame (make-frame))
  (toggle-frame-maximized)
  (vterm)
  (evil-insert-state))

(defun petmacs/cycle-theme ()
  "Cycle through a list of themes, petmacs-themes-list."
  (interactive)
  (when (not (member petmacs--default-theme petmacs-themes-list))
    (setq petmacs-themes-list (append petmacs-themes-list (list petmacs--default-theme))))
  (setq petmacs--default-theme (pop petmacs-themes-list))
  (load-theme  petmacs--default-theme t))

(defun petmacs/select-theme ()
  (interactive)
  (when (not (member petmacs--default-theme petmacs-themes-list))
    (setq petmacs-themes-list (append petmacs-themes-list (list petmacs--default-theme))))
  (counsel-load-theme))

(defun petmacs/switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(defun petmacs/ivy-persp-switch-project-advice (project)
  (let ((persp-reset-windows-on-nil-window-conf t))
    (persp-switch project)))

(defun petmacs/ivy-persp-switch-project (arg)
  (interactive "P")
  (require 'counsel-projectile)
  (advice-add 'counsel-projectile-switch-project-action
	      :before #'petmacs/ivy-persp-switch-project-advice)
  (ivy-read "Switch to Project Perspective: "
            (if (projectile-project-p)
                (cons (abbreviate-file-name (projectile-project-root))
		      (projectile-relevant-known-projects))
	      projectile-known-projects)
            :action #'counsel-projectile-switch-project-action
            :caller 'petmacs/ivy-persp-switch-project)
  (advice-remove 'counsel-projectile-switch-project-action
                 'petmacs/ivy-persp-switch-project-advice))

(defun petmacs/save-buffer-gbk-as-utf8 ()
  "Revert a buffer with GBK and save as UTF-8."
  (interactive)
  (save-buffer-as-utf8 'gbk))

(defun petmacs/evil-goto-definition-other-window ()
  "Jump to definition around point in other window."
  (interactive)
  (let ((pos (point)))
    (switch-to-buffer-other-window (current-buffer))
    (goto-char pos)
    (evil-goto-definition)))

;; Dos2Unix/Unix2Dos
(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun petmacs/copy-file ()
  "Write the file under new name."
  (interactive)
  (call-interactively 'write-file))

(defun petmacs/hidden-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings"
  (interactive)
  (unless buffer-display-table
    (setq buffer-display-table (make-display-table)))
  (aset buffer-display-table ?\^M []))

(defun petmacs/remove-dos-eol ()
  "Replace DOS eol CR LF with Unix eolns CR"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun petmacs/goto-dashboard ()
  "goto Home buffer"
  (interactive)
  (switch-to-buffer "*dashboard*"))

(defun petmacs/goto-scratch-buffer ()
  "goto Home buffer"
  (interactive)
  (switch-to-buffer "*scratch*"))

;; Revert buffer
(defun petmacs/revert-this-buffer ()
  "Revert the current buffer."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (revert-buffer t t)
    (message "Reverted this buffer.")))
(bind-key "<f5>" #'petmacs/revert-this-buffer)
(if sys/mac-x-p
    (bind-key "s-r" #'petmacs/revert-this-buffer))

(defun petmacs/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun petmacs/counsel-jump-in-buffer ()
  "Jump in buffer with `counsel-imenu' or `counsel-org-goto' if in org-mode"
  (interactive)
  (call-interactively
   (cond
    ((eq major-mode 'org-mode) 'counsel-org-goto)
    (t 'counsel-imenu))))

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


(defun petmacs--projectile-file-path ()
  "Retrieve the file path relative to project root.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not visit a file."
  (when-let (file-name (buffer-file-name))
    (file-relative-name (file-truename file-name) (projectile-project-root))))


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

(defun petmacs--file-path ()
  "Retrieve the file path of the current buffer.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (buffer-file-name))
    (file-truename file-path)))

(defun petmacs/copy-directory-path ()
  "Copy and show the directory path of the current buffer.

If the buffer is not visiting a file, use the `list-buffers-directory'
variable as a fallback to display the directory, useful in buffers like the
ones created by `magit' and `dired'."
  (interactive)
  (if-let (directory-path (petmacs--directory-path))
      (message "%s" (kill-new directory-path))
    (message "WARNING: Current buffer does not have a directory!")))

(defun petmacs/copy-file-path ()
  "Copy and show the file path of the current buffer."
  (interactive)
  (if-let (file-path (petmacs--file-path))
      (message "%s" (kill-new file-path))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun petmacs/copy-file-name ()
  "Copy and show the file name of the current buffer."
  (interactive)
  (if-let (file-name (file-name-nondirectory (petmacs--file-path)))
      (message "%s" (kill-new file-name))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun petmacs/projectile-copy-file-path ()
  "Copy and show the file path relative to project root."
  (interactive)
  (if-let (file-path (petmacs--projectile-file-path))
      (message "%s" (kill-new file-path))
    (message "WARNING: Current buffer is not visiting a file!")))

(defun petmacs/pop-eshell (arg)
  "Pop a shell in a side window.
Pass arg to ‘shell’."
  (interactive "P")
  (select-window
   (display-buffer-in-side-window
    (save-window-excursion
      (let ((prefix-arg arg))
        (call-interactively #'eshell))
      (current-buffer))
    '((side . right)
      (window-width . fit-window-to-buffer)))))


(defun petmacs/shell-pop ()
  "Open a term buffer at projectile project root."
  (interactive)
  (call-interactively 'shell-pop)
  (evil-insert-state))

(defun petmacs/projectile-pop-eshell ()
  "Open a term buffer at projectile project root."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (call-interactively 'petmacs/pop-eshell)))

(defun petmacs/projectile-shell-pop ()
  "Open a term buffer at projectile project root."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (call-interactively 'shell-pop)
    (evil-insert-state)))


;; Recompile site-lisp directory
(defun recompile-site-lisp ()
  "Recompile packages in site-lisp directory."
  (interactive)
  (let ((dir (concat user-emacs-directory "site-lisp")))
    (if (fboundp 'async-byte-recompile-directory)
        (async-byte-recompile-directory dir)
      (byte-recompile-directory dir 0 t))))

(defun petmacs/find-dotfile ()
  "Edit the `dotfile', in the current window."
  (interactive)
  (find-file-existing (concat user-emacs-directory "init.el")))

(defun petmacs/find-custom-file ()
  "Edit the `dotfile', in the current window."
  (interactive)
  (find-file-existing (concat user-emacs-directory "custom.el")))

(defun petmacs/find-org-global-todos ()
  "Edit the `dotfile', in the current window."
  (interactive)
  (let ((filename (expand-file-name "TODOs.org" org-directory)))
    (unless (file-exists-p filename)
      (make-directory org-directory t))
    (find-file filename)
    (if (file-exists-p filename)
	(revert-buffer nil t))))

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

(defun petmacs/frame-killer ()
  "Kill server buffer and hide the main Emacs window"
  (interactive)
  (condition-case nil
      (delete-frame nil 1)
    (error
     (make-frame-invisible nil 1))))



;;; Frame
(defvar petmacs-frame--geometry nil)
(defun petmacs-frame--save-geometry ()
  "Save current frame's geometry."
  (setq-local centaur-frame--geometry
              `((left . ,(frame-parameter nil 'left))
                (top . ,(frame-parameter nil 'top))
                (width . ,(frame-parameter nil 'width))
                (height . ,(frame-parameter nil 'height))
                (fullscreen))))

(defun petmacs-frame--fullscreen-p ()
  "Returns Non-nil if the frame is fullscreen."
  (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth)))

(defun petmacs-frame-maximize ()
  "Maximize the frame."
  (interactive)
  (petmacs-frame--save-geometry)
  (unless (eq (frame-parameter nil 'fullscreen) 'maximized)
    (set-frame-parameter nil 'fullscreen 'maximized)))

(defun petmacs-frame-restore ()
  "Restore the frame's size and position."
  (interactive)
  (modify-frame-parameters nil petmacs-frame--geometry))

(defun petmacs-frame-left-half ()
  "Put the frame to the left-half."
  (interactive)
  (unless (petmacs-frame--fullscreen-p)
    (petmacs-frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (/ (nth 2 attr) 2) 20))
           (height (- (nth 3 attr) 30))
           (left (nth 0 attr))
           (top (nth 1 attr)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

(defun petmacs-frame-right-half ()
  "Put the frame to the right-half."
  (interactive)
  (unless (petmacs-frame--fullscreen-p)
    (petmacs-frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (/ (nth 2 attr) 2) 20))
           (height (- (nth 3 attr) 30))
           (left (+ (nth 0 attr) width 20))
           (top (nth 1 attr)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

(defun petmacs-frame-top-half ()
  "Put the frame to the top-half."
  (interactive)
  (unless (petmacs-frame--fullscreen-p)
    (petmacs-frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (nth 2 attr) 20))
           (height (- (/ (nth 3 attr) 2) 30))
           (left (nth 0 attr))
           (top (nth 1 attr)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

(defun petmacs-frame-bottom-half ()
  "Put the frame to the bottom-half."
  (interactive)
  (unless (petmacs-frame--fullscreen-p)
    (petmacs-frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (nth 2 attr) 20))
           (height (- (/ (nth 3 attr) 2) 30))
           (left (nth 0 attr))
           (top (+ (nth 1 attr) height 30)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

;;; python

(defun petmacs/pyenv-executable-find (command)
  "Find executable taking pyenv shims into account.
If the executable is a system executable and not in the same path
as the pyenv version then also return nil. This works around https://github.com/pyenv/pyenv-which-ext
"
  (if (executable-find "pyenv")
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

(defun petmacs/python-start-or-switch-repl ()
  "Start and/or switch to the REPL."
  (interactive)
  (let ((shell-process
         (or (python-shell-get-process)
             ;; `run-python' has different return values and different
             ;; errors in different emacs versions. In 24.4, it throws an
             ;; error when the process didn't start, but in 25.1 it
             ;; doesn't throw an error, so we demote errors here and
             ;; check the process later
             (with-demoted-errors "Error: %S"
               ;; in Emacs 24.5 and 24.4, `run-python' doesn't return the
               ;; shell process
               (call-interactively #'run-python)
               (python-shell-get-process)))))
    (unless shell-process
      (error "Failed to start python shell properly"))
    (pop-to-buffer (process-buffer shell-process))
    (evil-insert-state)))

(defun petmacs/python-shell-send-buffer-switch ()
  "Send buffer content to shell and switch to it in insert mode."
  (interactive)
  (let ((python-mode-hook nil))
    (python-shell-send-buffer)
    (python-shell-switch-to-shell)
    (evil-insert-state)))

(defun petmacs/python-shell-send-buffer ()
  "Send buffer content to shell and switch to it in insert mode."
  (interactive)
  (let ((python-mode-hook nil))
    (python-shell-send-buffer)))

(defun petmacs/python-shell-send-defun-switch ()
  "Send function content to shell and switch to it in insert mode."
  (interactive)
  (let ((python-mode-hook nil))
    (python-shell-send-defun nil)
    (python-shell-switch-to-shell)
    (evil-insert-state)))

(defun petmacs/python-shell-send-defun ()
  "Send function content to shell and switch to it in insert mode."
  (interactive)
  (let ((python-mode-hook nil))
    (python-shell-send-defun nil)))

(defun petmacs/python-shell-send-region-switch (start end)
  "Send region content to shell and switch to it in insert mode."
  (interactive "r")
  (let ((python-mode-hook nil))
    (python-shell-send-region start end)
    (python-shell-switch-to-shell)
    (evil-insert-state)))

(defun petmacs/python-shell-send-region (start end)
  "Send region content to shell and switch to it in insert mode."
  (interactive "r")
  (let ((python-mode-hook nil))
    (python-shell-send-region start end)))

(defun petmacs/python-load-venv-file ()
  "Set pyvenv virtualenv from \".venv\" by looking in parent directories. handle directory or file"
  (interactive)
  (let ((root-path (locate-dominating-file default-directory
                                           ".venv")))
    (when root-path
      (let* ((file-path (expand-file-name ".venv" root-path))
             (virtualenv
              (if (file-directory-p file-path)
                  file-path
                (with-temp-buffer
                  (insert-file-contents-literally file-path)
                  (buffer-substring-no-properties (line-beginning-position)
                                                  (line-end-position))))))))))

;; from https://www.snip2code.com/Snippet/127022/Emacs-auto-remove-unused-import-statemen
(defun petmacs/python-remove-unused-imports()
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

(defun petmacs/python-quit-repl ()
  (interactive)
  (switch-to-buffer "*Python*")
  (comint-quit-subjob)
  (kill-buffer-and-window))

(defun petmacs/python-interrupt-repl ()
  (interactive)
  (switch-to-buffer "*Python*")
  (comint-interrupt-subjob)
  (other-window -1))

(defun petmacs/pyvenv-workon ()
  "switch python virtualenvironment and restart anaconda server"
  (interactive)
  (call-interactively 'pyvenv-workon)
  (pythonic-activate pyvenv-virtual-env))

(defun petmacs/pyvenv-activate ()
  "switch python virtualenvironment and restart anaconda server"
  (interactive)
  (call-interactively 'pyvenv-activate)
  (pythonic-activate pyvenv-virtual-env))

(defun petmacs/pyvenv-deactivate ()
  "deactivate pyvenv & anaconda virtual enironment"
  (interactive)
  (pyvenv-deactivate)
  (pythonic-deactivate))

(defun petmacs/python-highlight-breakpoint ()
  "highlight a break point"
  (interactive)
  ;; (highlight-lines-matching-regexp "^[ ]*import ipdb" 'hi-pink)
  ;; (highlight-lines-matching-regexp "^[ ]*ipdb.set_trace()" 'hi-pink)
  (highlight-lines-matching-regexp "breakpoint()" 'hi-pink)
  (highlight-lines-matching-regexp "import \\(pdb\\|ipdb\\|pudb\\|wdb\\)" 'hi-pink)
  (highlight-lines-matching-regexp "\\(pdb\\|ipdb\\|pudb\\|wdb\\).set_trace()" 'hi-pink)
  (highlight-lines-matching-regexp "trepan.api.debug()" 'hi-pink)
  )

(defun petmacs/python-insert-breakpoint ()
  "Add a break point, highlight it."
  (interactive)
  (let ((trace  "import ipdb; ipdb.set_trace() # XXX BREAKPOINT")
	(line (thing-at-point 'line)))
    (if (and line (string-match trace line))
	(kill-whole-line)
      (progn
	(back-to-indentation)
	(insert trace)
	(insert "\n")
	(python-indent-line)
	(petmacs/python-highlight-breakpoint)))))

(defun petmacs/python-delete-breakpoint ()
  "delete break point"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (flush-lines "^[ ]*import ipdb")
    (flush-lines "^[ ]*ipdb.set_trace()")))


(defun petmacs/error-delegate ()
  "Decide which error API to delegate to.

Delegates to flycheck if it is enabled and the next-error buffer
is not visible. Otherwise delegates to regular Emacs next-error."
  (if (and (bound-and-true-p flycheck-mode)
           (let ((buf (ignore-errors (next-error-find-buffer))))
             (not (and buf (get-buffer-window buf)))))
      'flycheck
    'emacs))

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


(defun petmacs/treemacs-project-toggle ()
  "Toggle and add the current project to treemacs if not already added."
  (interactive)
  (if (eq (treemacs-current-visibility) 'visible)
      (delete-window (treemacs-get-local-window))
    (let ((path (projectile-project-root))
          (name (projectile-project-name)))
      (unless (treemacs-current-workspace)
        (treemacs--find-workspace))
      (treemacs-do-add-project-to-workspace path name)
      (treemacs-select-window))))


(defun petmacs/toggle-flycheck-error-list ()
  "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
  (interactive)
  (-if-let (window (flycheck-get-error-list-window))
      (quit-window nil window)
    (flycheck-list-errors)))

(defun petmacs/open-mintty-terminal-here ()
  (interactive)
  (call-process-shell-command  "mintty /bin/env MSYSTEM=MINGW64 CHERE_INVOKING=1 /bin/bash --login -i &" nil 0))

;;; markdown

;; Insert key for org-mode and markdown a la C-h k
;; from SE endless http://emacs.stackexchange.com/questions/2206/i-want-to-have-the-kbd-tags-for-my-blog-written-in-org-mode/2208#2208
(defun petmacs/insert-keybinding-markdown (key)
  "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
  (interactive "kType key sequence: ")
  (let* ((tag "~%s~"))
    (if (null (equal key "\r"))
        (insert
         (format tag (help-key-description key nil)))
      (insert (format tag ""))
      (forward-char -6))))

;;; macros
(defmacro petmacs|org-emphasize (fname char)
  "Make function for setting the emphasis in org mode"
  `(defun ,fname () (interactive)
      (org-emphasize ,char)))

;;; hydra

(defhydra hydra-frame-window (:color pink :hint nil)
  "
^Frame^                 ^Window^      ^Window Size^^^^     ^Text Zoom^
^^----------------------^^------------^^----------^^^^-----^^---------------         (__)
_0_: delete             _t_oggle        ^ ^ _k_ ^ ^            _+_                   (oo)
_1_: delete others      _s_wap          _h_ ^+^ _l_            _=_             /------\\/
_2_: new                _d_elete        ^ ^ _j_ ^ ^            _-_            / |    ||
_F_ullscreen            _o_ther         _b_alance^^^^          ^ ^         *  /\\-----/\\  ~~  C-c w/C-x o w
"
  ("0" delete-frame :exit t)
  ("1" delete-other-frames :exit t)
  ("2" make-frame  :exit t)
  ("b" balance-windows)
  ("s" ace-swap-window)
  ("F" toggle-frame-fullscreen)
  ("t" toggle-window-split)
  ("d" ace-delete-window :exit t)
  ("o" ace-window :exit t)
  ("-" text-scale-decrease)
  ("=" (text-scale-increase 0))
  ("+" text-scale-increase)
  ("h" shrink-window-horizontally)
  ("k" shrink-window)
  ("j" enlarge-window)
  ("l" enlarge-window-horizontally)
  ("q" nil "quit"))


(defhydra org-babel-transient-state (:color pink :hint nil)
  "
[_j_/_k_] navigate src blocks         [_e_] execute src block
[_g_]^^   goto named block            [_'_] edit src block
[_z_]^^   recenter screen             [_q_] quit"
  ("q" nil :exit t)
  ("j" org-babel-next-src-block)
  ("k" org-babel-previous-src-block)
  ("g" org-babel-goto-named-src-block)
  ("z" recenter-top-bottom)
  ("e" org-babel-execute-maybe :exit t)
  ("'" org-edit-special :exit t))

(defhydra org-agenda-transient-state (:color pink :hint nil)
  "
Headline^^            Visit entry^^               Filter^^                    Date^^                  Toggle mode^^        View^^             Clock^^        Other^^
--------^^---------   -----------^^------------   ------^^-----------------   ----^^-------------     -----------^^------  ----^^---------    -----^^------  -----^^-----------
[_ht_] set status     [_SPC_] in other window     [_ft_] by tag               [_ds_] schedule         [_tf_] follow        [_vd_] day         [_cI_] in      [_gr_] reload
[_hk_] kill           [_TAB_] & go to location    [_fr_] refine by tag        [_dS_] un-schedule      [_tl_] log           [_vw_] week        [_cO_] out     [_._]  go to today
[_hr_] refile         [_RET_] & del other windows [_fc_] by category          [_dd_] set deadline     [_ta_] archive       [_vt_] fortnight   [_cq_] cancel  [_gd_] go to date
[_hA_] archive        [_o_]   link                [_fh_] by top headline      [_dD_] remove deadline  [_tr_] clock report  [_vm_] month       [_cj_] jump    ^^
[_h:_] set tags       ^^                          [_fx_] by regexp            [_dt_] timestamp        [_ti_] clock issues  [_vy_] year        ^^             ^^
[_hp_] set priority   ^^                          [_fd_] delete all filters   [_+_]  do later         [_td_] diaries       [_vn_] next span   ^^             ^^
^^                    ^^                          ^^                          [_-_]  do earlier       ^^                   [_vp_] prev span   ^^             ^^
^^                    ^^                          ^^                          ^^                      ^^                   [_vr_] reset       ^^             ^^
[_q_] quit
"
  ;; Entry
  ("h:" org-agenda-set-tags)
  ("hA" org-agenda-archive-default)
  ("hk" org-agenda-kill)
  ("hp" org-agenda-priority)
  ("hr" org-agenda-refile)
  ("ht" org-agenda-todo)

  ;; Visit entry
  ("SPC" org-agenda-show-and-scroll-up)
  ("<tab>" org-agenda-goto :exit t)
  ("TAB" org-agenda-goto :exit t)
  ("RET" org-agenda-switch-to :exit t)
  ("o"   link-hint-open-link :exit t)

  ;; Date
  ("ds" org-agenda-schedule)
  ("dS" (lambda () (interactive)
          (let ((current-prefix-arg '(4)))
            (call-interactively 'org-agenda-schedule))))
  ("dd" org-agenda-deadline)
  ("dt" org-agenda-date-prompt)
  ("dD" (lambda () (interactive)
          (let ((current-prefix-arg '(4)))
            (call-interactively 'org-agenda-deadline))))
  ("+" org-agenda-do-date-later)
  ("-" org-agenda-do-date-earlier)

  ;; View
  ("vd" org-agenda-day-view)
  ("vw" org-agenda-week-view)
  ("vt" org-agenda-fortnight-view)
  ("vm" org-agenda-month-view)
  ("vy" org-agenda-year-view)
  ("vn" org-agenda-later)
  ("vp" org-agenda-earlier)
  ("vr" org-agenda-reset-view)

  ;; Toggle mode
  ("tf" org-agenda-follow-mode)
  ("tl" org-agenda-log-mode)
  ("ta" org-agenda-archives-mode)
  ("tr" org-agenda-clockreport-mode)
  ("ti" org-agenda-show-clocking-issues)
  ("td" org-agenda-toggle-diary)

  ;; Filter
  ("ft" org-agenda-filter-by-tag)
  ("fr" org-agenda-filter-by-tag-refine)
  ("fc" org-agenda-filter-by-category)
  ("fh" org-agenda-filter-by-top-headline)
  ("fx" org-agenda-filter-by-regexp)
  ("fd" org-agenda-filter-remove-all)

  ;; Clock
  ("cI" org-agenda-clock-in :exit t)
  ("cj" org-agenda-clock-goto :exit t)
  ("cO" org-agenda-clock-out)
  ("cq" org-agenda-clock-cancel)

  ;; Other
  ("q" nil :exit t)
  ("gr" org-agenda-redo)
  ("." org-agenda-goto-today)
  ("gd" org-agenda-goto-date))

(provide 'core-funcs)

;;; core-funcs.el ends here
