;; init-misc.el --- Setup useful packages.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package default-text-scale
  :init
  (default-text-scale-mode))

(use-package beacon
  :custom
  (beacon-color "yellow")
  :init
  (beacon-mode 1))

(provide 'init-misc)

;;; init-misc.el ends here
