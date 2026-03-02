;;; init-package.el --- Initialize package configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs Package management configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom)
  (require 'init-funcs))

;; Suppress warnings
(defvar use-package-always-ensure)
(defvar use-package-always-defer)
(defvar use-package-expand-minimally)
(defvar use-package-enable-imenu-support)

;; HACK: DO NOT save `package-selected-packages' to `custom-file'
;; @see https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun my/package--save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to custom.el."
  (when (or value after-init-time)
    ;; It is valid to set it to nil, for example when the last package
    ;; is uninstalled.  But it shouldn't be done at init time, to
    ;; avoid overwriting configurations that haven't yet been loaded.
    (setq package-selected-packages (sort value #'string<)))
  (unless after-init-time
    (add-hook 'after-init-hook #'my/package--save-selected-packages)))
(advice-add 'package--save-selected-packages :override #'my/package--save-selected-packages)

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; Prettify package list
(set-face-attribute 'package-status-available nil :inherit 'font-lock-string-face)
(set-face-attribute 'package-description nil :inherit 'font-lock-comment-face)

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; ;; Should set before loading `use-package'
(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-expand-minimally t
      use-package-enable-imenu-support t)

;; (eval-when-compile
;;   (require 'use-package))

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)

;; Update packages
(unless (fboundp 'package-upgrade-all)
  (use-package auto-package-update
    :init
    (setq auto-package-update-delete-old-versions t
          auto-package-update-hide-results t)
    (defalias 'package-upgrade-all #'auto-package-update-now)))

;; Update GPG keyring for GNU ELPA
;; (use-package gnu-elpa-keyring-update)

(provide 'init-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
