;; init-package.el --- Setup download / update packages.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; HACK: DO NOT copy package-selected-packages to init/custom file forcibly.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun my-save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to `custom-file'."
  (when value
    (setq package-selected-packages value)))
(advice-add 'package--save-selected-packages :override #'my-save-selected-packages)

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

(use-package quelpa
  :init
  (setq quelpa-self-upgrade-p nil)
  (setq quelpa-update-melpa-p nil)
  (setq quelpa-checkout-melpa-p nil))

;; Required by `use-package'
(use-package quelpa-use-package
  :init
  (require 'quelpa-use-package)
  (quelpa-use-package-activate-advice))

(use-package diminish)
(use-package bind-key)
;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

(use-package page-break-lines
  :commands (page-break-lines-mode))

;; Extensions 
;; download / update packages
(use-package paradox
  :ensure t
  :commands paradox-enable
  :hook (after-init . paradox-enable)
  :init
  (setq paradox-execute-asynchronously t
	paradox-spinner-type 'progress-bar
	paradox-github-token t
	paradox-display-star-count nil)

  (defalias #'upgrade-packages #'paradox-upgrade-packages)

  ;; Replace default `list-packages'
  (defun my-paradox-enable (&rest _)
    "Enable paradox, overriding the default package-menu."
    (paradox-enable))
  (advice-add #'list-packages :before #'my-paradox-enable)
  :config
  (when (fboundp 'page-break-lines-mode)
    (add-hook 'paradox-after-execute-functions
              (lambda (&rest _)
                (let ((buf (get-buffer-create "*Paradox Report*"))
                      (inhibit-read-only t))
                  (with-current-buffer buf
                    (page-break-lines-mode 1))))
              t)))

(provide 'init-package)

;;; init-package.el ends here
