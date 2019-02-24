;; init-package.el --- Setup download / update packages.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

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

(use-package quelpa)
(setq quelpa-self-upgrade-p nil)
(setq quelpa-update-melpa-p nil)
(setq quelpa-checkout-melpa-p nil)

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://framagit.org/steckerhalter/quelpa-use-package.git"))
(require 'quelpa-use-package)
;; disable :ensure from package.el when :quelpa is found in use-package
(quelpa-use-package-activate-advice)

;; Extensions 
;; download / update packages
(use-package paradox
  :ensure t
  :commands paradox-enable
  :hook (after-init . paradox-enable)
  :init
  (setq paradox-execute-asynchronously t)
  (setq paradox-github-token t)
  (defalias 'upgrade-packages 'paradox-upgrade-packages))

(provide 'init-package)

;;; init-package.el ends here
