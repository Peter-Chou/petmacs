;; -*- lexical-binding: t no-byte-compile: t -*-

(require 'cl-lib)

(require 'init-const)
(require 'init-custom)

;; Suppress warnings
(defvar socks-noproxy)
(defvar socks-server)

(declare-function chart-bar-quickie 'chart)
(declare-function flymake-start 'flymake)
(declare-function flycheck-buffer 'flycheck)
(declare-function async-inject-variables 'async)
(declare-function imenu-list-smart-toggle 'imenu-list)
(declare-function symbols-outline-show 'symbols-outline)
(declare-function upgrade-packages 'init-package)

(defun petmacs/imenu-list-smart-toggle ()
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (symbols-outline-show)
    (imenu-list-smart-toggle)))

(defun petmacs/open-gnome-terminal ()
  (interactive)
  (shell-command "gnome-terminal"))

(unless (fboundp 'caadr)
  (defalias 'caadr #'cl-caadr))

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (and petmacs-icon
       (or (featurep 'nerd-icons)
           (require 'nerd-icons nil t))))

(defun petmacs/candidate-show-framework ()
  (if (display-graphic-p)
      'posframe
    'popup))

;; Dos2Unix/Unix2Dos
(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun delete-carrage-returns ()
  "Delete `^M' characters in the buffer.
Same as `replace-string C-q C-m RET RET'."
  (interactive)
  (save-excursion
    (goto-char 0)
    (while (search-forward "\r" nil :noerror)
      (replace-match ""))))

;; File and buffer
(defun petmacs/revert-this-buffer ()
  "Revert the current buffer."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (revert-buffer t t)
    (message "Reverted this buffer")))

(defun petmacs/goto-scratch-buffer ()
  "goto Home buffer"
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun petmacs/goto-dashboard ()
  "goto Home buffer"
  (interactive)
  (switch-to-buffer "*dashboard*"))

(defun petmacs/goto-org-global-schedules ()
  "Edit the `dotfile', in the current window."
  (interactive)
  (let ((filename (expand-file-name "schedules.org" (concat user-emacs-directory "data/gtd"))))
    (find-file filename)
    (if (file-exists-p filename)
	    (revert-buffer nil t))))

(defun petmacs/goto-org-global-todos ()
  "Edit the `dotfile', in the current window."
  (interactive)
  (let ((filename (expand-file-name "TODO.org" (concat user-emacs-directory "data/gtd"))))
    (find-file filename)
    (if (file-exists-p filename)
	    (revert-buffer nil t))))

(defun petmacs/switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(defun petmacs/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun petmacs/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;; Mode line
(defun mode-line-height ()
  "Get the height of the mode-line."
  (- (elt (window-pixel-edges) 3)
     (elt (window-inside-pixel-edges) 3)))

	;; Reload configurations
(defun petmacs/reload-init-file ()
  "Reload Emacs configurations."
  (interactive)
  (load user-init-file))

;; theme

(defun petmacs--load-theme (theme)
  "Disable others and enable new one."
  (when theme
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)
    (message "Loaded theme `%s'" theme)))

(defun save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

(defun save-buffer-gbk-as-utf8 ()
  "Revert a buffer with GBK and save as UTF-8."
  (interactive)
  (save-buffer-as-utf8 'gbk))

(defun byte-compile-elpa ()
  "Compile packages in elpa directory. Useful if you switch Emacs versions."
  (interactive)
  (if (fboundp 'async-byte-recompile-directory)
      (async-byte-recompile-directory package-user-dir)
    (byte-recompile-directory package-user-dir 0 t)))

(defun byte-compile-site-lisp ()
  "Compile packages in site-lisp directory."
  (interactive)
  (let ((dir (locate-user-emacs-file "site-lisp")))
    (if (fboundp 'async-byte-recompile-directory)
        (async-byte-recompile-directory dir)
      (byte-recompile-directory dir 0 t))))

(defun native-compile-elpa ()
  "Native-compile packages in elpa directory."
  (interactive)
  (if (fboundp 'native-compile-async)
      (native-compile-async package-user-dir t)))

(defun native-compile-site-lisp ()
  "Native compile packages in site-lisp directory."
  (interactive)
  (let ((dir (locate-user-emacs-file "site-lisp")))
    (if (fboundp 'native-compile-async)
        (native-compile-async dir t))))

(defun petmacs-treesit-available-p ()
  "Check whether tree-sitter is available.
Native tree-sitter is introduced since 29."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)))

;; Frame
(defvar petmacs-frame--geometry nil)
(defun petmacs-frame--save-geometry ()
  "Save current frame's geometry."
  (setq-local petmacs-frame--geometry
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

;; Network Proxy
(defun proxy-http-show ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is `%s'" petmacs-proxy)
    (message "No HTTP proxy")))

(defun proxy-http-enable ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,petmacs-proxy)
          ("https" . ,petmacs-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (proxy-http-show))

(defun proxy-http-disable ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (proxy-http-show))

(defun proxy-http-toggle ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if (bound-and-true-p url-proxy-services)
      (proxy-http-disable)
    (proxy-http-enable)))

(defun proxy-socks-show ()
  "Show SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (message "Current SOCKS%d proxy is %s:%s"
               (cadddr socks-server) (cadr socks-server) (caddr socks-server))
    (message "No SOCKS proxy")))

(defun proxy-socks-enable ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (setq url-gateway-method 'socks
        socks-noproxy '("localhost"))
  (let* ((proxy (split-string petmacs-socks-proxy ":"))
         (host (car proxy))
         (port (string-to-number (cadr proxy))))
    (setq socks-server `("Default server" ,host ,port 5)))
  (setenv "all_proxy" (concat "socks5://" petmacs-socks-proxy))
  (proxy-socks-show))

(defun proxy-socks-disable ()
  "Disable SOCKS proxy."
  (interactive)
  (setq url-gateway-method 'native
        socks-noproxy nil
        socks-server nil)
  (setenv "all_proxy" "")
  (proxy-socks-show))

(defun proxy-socks-toggle ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (proxy-socks-disable)
    (proxy-socks-enable)))

;; WORKAROUND: fix blank screen issue on macOS.
(defun fix-fullscreen-cocoa ()
  "Address blank screen issue with child-frame in fullscreen.
This issue has been addressed in 28."
  (and sys/mac-cocoa-p
       (not emacs/>=28p)
       (bound-and-true-p ns-use-native-fullscreen)
       (setq ns-use-native-fullscreen nil)))

(defun petmacs/frame-killer ()
  "Kill server buffer and hide the main Emacs window"
  (interactive)
  (condition-case nil
      (delete-frame nil 1)
    (error
     (make-frame-invisible nil 1))))

(defun too-long-file-p ()
  "Check whether the file is too long."
  (if (fboundp 'buffer-line-statistics)
      (> (car (buffer-line-statistics)) 3000)
    (> (buffer-size) 100000)))

(defun childframe-workable-p ()
  "Whether childframe is workable."
  (not (or noninteractive
           emacs-basic-display
           (not (display-graphic-p)))))

(defun childframe-completion-workable-p ()
  "Whether childframe completion is workable."
  (and (eq petmacs-completion-style 'childframe)
       (childframe-workable-p)))

(provide 'init-funcs)
