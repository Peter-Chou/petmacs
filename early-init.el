;;; early-init.el --- Early initialization. -*- lexical-binding: t no-byte-compile: t -*-

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Prefer loading newest compiled .el file
(customize-set-variable 'load-prefer-newer noninteractive)

;; use mirror
(setq package-archives '(
                         ;; ("gnu"    . "https://elpa.gnu.org/packages/")
                         ;; ("melpa"  . "https://melpa.org/packages/")
                         ;; ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ;; ("org"    . "https://orgmode.org/elpa/")

                         ("gnu"    . "http://1.15.88.122/gnu/")
                         ("melpa"  . "http://1.15.88.122/melpa/")
                         ("nongnu" . "http://1.15.88.122/nongnu/")
                         ("org"    . "http://1.15.88.122/org/")
                         ))

(if (functionp 'json-serialize)
    (message "Native JSON is available")
  (message "Native JSON is *not* available"))

(if (and (fboundp 'native-comp-available-p)
	     (native-comp-available-p))
    (progn
      (message "Native compilation is available")
      (setq package-native-compile t
	        native-comp-async-report-warnings-errors nil
            ;; Make native compilation happens asynchronously
            native-comp-deferred-compilation nil ;; obsolete since 29.1
            native-comp-jit-compilation nil))
  (message "Native complation is *not* available"))

(setq byte-compile-warnings nil)

(setq package-enable-at-startup nil)

;; `use-package' is builtin since 29.
;; It must be set before loading `use-package'.
(setq use-package-enable-imenu-support t)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; (when (fboundp 'startup-redirect-eln-cache)
;;   (startup-redirect-eln-cache
;;    (convert-standard-filename
;; 	(expand-file-name  "data/eln-cache" user-emacs-directory)))
;;   (add-to-list 'native-comp-eln-load-path (expand-file-name "data/eln-cache" user-emacs-directory)))

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
(setq-default mode-line-format nil)

;; workaround image-type: Invalid image type svg
(add-to-list 'image-types 'svg)
