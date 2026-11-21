;; init-const.el --- Define constants.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Define constants.
;;

;;; Code:

(defconst petmacs-custom-example-file
  (expand-file-name "custom-example.el" user-emacs-directory)
  "Custom example file of Petmacs Emacs.")

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/unixp
  (eq system-type 'berkeley-unix)
  "Are we running on an Unix system?")

(defconst sys/wslp
  (and sys/linuxp
       (or (getenv "WSL_DISTRO_NAME")
           (getenv "WSL_INTEROP")
           (and (file-readable-p "/proc/version")
                (with-temp-buffer
                  (insert-file-contents "/proc/version")
                  (let ((case-fold-search t))
                    (string-match-p "microsoft\\|wsl" (buffer-string))))))
       t)
  "Are we running under Windows Subsystem for Linux (WSL/WSLg)?

WSL exports `WSL_DISTRO_NAME' and `WSL_INTEROP'; the kernel version is
used as a fallback for old WSL1 sessions.  `wl-copy' is *not* a WSL
marker: native Wayland sessions select it as well.")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/mac-ns-p
  (eq window-system 'ns)
  "Are we running on a GNUstep or Macintosh Cocoa display?")

(defconst sys/mac-cocoa-p
  (featurep 'cocoa)
  "Are we running with Cocoa on a Mac system?")

(defconst sys/mac-port-p
  (eq window-system 'mac)
  "Are we running a macport build on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst emacs/>=29p
  (>= emacs-major-version 29)
  "Emacs is 29 or above.")

(defconst emacs/>=29.2p
  (version<= "29.2" emacs-version)
  "Emacs is 29.2 or above.")

(defconst emacs/>=30p
  (>= emacs-major-version 30)
  "Emacs is 30 or above.")

(defconst emacs/>=31p
  (>= emacs-major-version 31)
  "Emacs is 31 or above.")

(defconst emacs/>=32p
  (>= emacs-major-version 32)
  "Emacs is 32 or above.")

(provide 'init-const)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-const.el ends here
