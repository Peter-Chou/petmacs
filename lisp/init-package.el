;; -*- lexical-binding: t no-byte-compile: t -*-

(setq package-user-dir (expand-file-name "data/elpa" user-emacs-directory))

;; HACK: DO NOT save package-selected-packages to `custom-file'.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun my-package--save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to `custom-file'."
  (when value
    (setq package-selected-packages value))
  (unless after-init-time
    (add-hook 'after-init-hook #'my-package--save-selected-packages)))
(advice-add 'package--save-selected-packages :override #'my-package--save-selected-packages)


;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

;; A modern Packages Menu
(use-package paradox
  :custom-face
  (paradox-archive-face ((t (:inherit font-lock-doc-face))))
  (paradox-description-face ((t (:inherit completions-annotations))))
  :hook (after-init . paradox-enable)
  :init (setq paradox-execute-asynchronously t
              paradox-github-token t
              paradox-display-star-count nil
              paradox-status-face-alist ;
              '(("built-in"  . font-lock-builtin-face)
                ("available" . success)
                ("new"       . (success bold))
                ("held"      . font-lock-constant-face)
                ("disabled"  . font-lock-warning-face)
                ("avail-obso" . font-lock-comment-face)
                ("installed" . font-lock-comment-face)
                ("dependency" . font-lock-comment-face)
                ("incompat"  . font-lock-comment-face)
                ("deleted"   . font-lock-comment-face)
                ("unsigned"  . font-lock-warning-face)))
  :config
  (when (fboundp 'page-break-lines-mode)
    (add-hook 'paradox-after-execute-functions
              (lambda (&rest _)
                "Display `page-break-lines' in \"*Paradox Report*\"."
                (let ((buf (get-buffer "*Paradox Report*"))
                      (inhibit-read-only t))
                  (when (buffer-live-p buf)
                    (with-current-buffer buf
                      (page-break-lines-mode 1)))))
              t)))

;; Auto update packages
(use-package auto-package-update
  :init
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t)
  (defalias 'upgrade-packages #'auto-package-update-now))

(use-package quelpa
  :init
  (setq quelpa-self-upgrade-p nil
        quelpa-update-melpa-p nil
        quelpa-checkout-melpa-p t
        quelpa-dir (expand-file-name "data/quelpa" user-emacs-directory)))

;; Required by `use-package'
(use-package quelpa-use-package
  :init
  (require 'quelpa-use-package)
  (quelpa-use-package-activate-advice))

(provide 'init-package)
