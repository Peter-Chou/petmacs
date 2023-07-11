;;; early-init.el --- Early initialization. -*- lexical-binding: t no-byte-compile: t -*-

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Prefer loading newest compiled .el file
(customize-set-variable 'load-prefer-newer noninteractive)

;; use mirror
(setq package-archives '(
                         ;; ("gnu" . "https://mirrors.163.com/elpa/gnu/")
                         ;; ("melpa" . "https://mirrors.163.com/elpa/melpa/")
                         ;; ("nongnu" . "https://mirrors.163.com/elpa/nongnu/")

                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")

                         ("org" . "https://orgmode.org/elpa/")
                         ))

(if (functionp 'json-serialize)
    (message "Native JSON is available")
  (message "Native JSON is *not* available"))

(if (and (fboundp 'native-comp-available-p)
	     (native-comp-available-p))
    (progn
      (message "Native compilation is available")
      ;; native-compile all Elisp files under a site-lisp/local directory
      (native-compile-async (expand-file-name "site-lisp/local" user-emacs-directory) 'recursively)
      (setq package-native-compile t
	        native-comp-async-report-warnings-errors nil
            ;; Make native compilation happens asynchronously
            native-comp-deferred-compilation nil))
  (message "Native complation is *not* available"))

(setq byte-compile-warnings nil)

(setq package-enable-at-startup nil)

(setq use-package-enable-imenu-support t)

(setq load-prefer-newer noninteractive)

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
	(expand-file-name  "data/eln-cache" user-emacs-directory)))
  (add-to-list 'native-comp-eln-load-path (expand-file-name "data/eln-cache" user-emacs-directory))
  )

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;; workaround image-type: Invalid image type svg
(add-to-list 'image-types 'svg)

(setq-default mode-line-format nil)
