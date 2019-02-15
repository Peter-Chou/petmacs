
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;; Initialize packages
(package-initialize)

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  ;; (setq use-package-always-ensure t)
  ;; (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

;; Required by `use-package'
(use-package diminish
	:ensure t)
(use-package bind-key
	:ensure t)

;; Extensions
(use-package paradox
  :ensure t
  :commands paradox-enable
  :hook (after-init . paradox-enable)
  :init
  (setq paradox-execute-asynchronously t)
  (setq paradox-github-token t)
  (defalias 'upgrade-packages 'paradox-upgrade-packages))

(provide 'init-package)

