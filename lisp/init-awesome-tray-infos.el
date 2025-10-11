;; init-awesome-tray-infos.el --- awesome-tray infos -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:
;;
;; custom infos for awesome-tray.el
;;

;;; Code:

(require 'awesome-tray)

(defun awesome-tray-project-relative-dir-info ()
  "Get relative directory info."
  (format "%s" (petmacs/get-project-relateive-dir)))
(add-to-list 'awesome-tray-module-alist '("project-relative-dir" . (awesome-tray-project-relative-dir-info awesome-tray-module-parent-dir-face)))

(defun awesome-tray-module-pomodoro-info ()
  "Get pomodoro info."
  (format "%s" pomodoro-mode-line-string))
(defface awesome-tray-module-pomodoro-face
  '((((background light))
     :foreground "#008080" :bold t)
    (t
     :foreground "#00ced1" :bold t))
  "Pomodoro face."
  :group 'awesome-tray)
(add-to-list 'awesome-tray-module-alist '("pomodoro" . (awesome-tray-module-pomodoro-info awesome-tray-module-pomodoro-face)))

(defun awesome-tray-module-pyvenv-info ()
  "Get pyvenv info."
  (if (and (member major-mode '(python-mode python-ts-mode)) (bound-and-true-p pyvenv-workon))
      ;; (format "<%s>" pyvenv-workon)
      (concat (format "%s " (nerd-icons-faicon "nf-fae-python")) (format "%s" pyvenv-workon))
    ""))
(defface awesome-tray-module-pyvenv-face
  '((((background light))
     :foreground "#0673d8" :bold t)
    (t
     :foreground "#369bf7" :bold t))
  "Pyvenv face."
  :group 'awesome-tray)
(add-to-list 'awesome-tray-module-alist '("pyvenv" . (awesome-tray-module-pyvenv-info awesome-tray-module-pyvenv-face)))

(defun awesome-tray-module-which-function-info ()
  "Get which function info."
  (if (derived-mode-p 'prog-mode)
      ;; (format "<%s>" pyvenv-workon)
      ;; (concat (format "%s " (nerd-icons-codicon "nf-cod-triangle_right")) (format "%s" (which-function)))
      (format "%s %s" (nerd-icons-codicon "nf-cod-triangle_right") (which-function))
    ""))
(defface awesome-tray-module-which-function-face
  '((((background light))
     :foreground "#0673d8" :bold t)
    (t
     :foreground "#369bf7" :bold t))
  "Pyvenv face."
  :group 'awesome-tray)
(add-to-list 'awesome-tray-module-alist '("which-function" . (awesome-tray-module-which-function-info awesome-tray-module-which-function-face)))

(defun awesome-tray-module-breadcrumbs-info ()
  "Get breadcrumbs info."
  (breadcrumb-imenu-crumbs))
(defface awesome-tray-module-breadcrumbs-face
  `((((background light))
     :foreground ,petmacs-favor-color :bold t)
    (t
     :foreground ,petmacs-favor-color :bold t))
  "Breadcrumbs face."
  :group 'awesome-tray)
(add-to-list 'awesome-tray-module-alist '("breadcrumbs" . (awesome-tray-module-breadcrumbs-info awesome-tray-module-breadcrumbs-face)))

(defun petmacs/awesome-tray-module-flymake-info ()
  "A module for showing Flymake state.(use custom unicode)."
  (with-demoted-errors
      ""
    (if (and (featurep 'flymake) flymake--state)
        (let* ((known (hash-table-keys flymake--state))
               (running (flymake-running-backends))
               (disabled (flymake-disabled-backends))
               (reported (flymake-reporting-backends))
               (disabledp (and disabled (null running)))
               (waiting (cl-set-difference running reported)))
          (when-let
              ((flymake-state
                (cond
                 (waiting (format "%s" (nerd-icons-mdicon "nf-md-timer_sand")))
                 ((null known) (format "%s" (nerd-icons-faicon "nf-fa-question")))
                 (disabledp (format "%s" (nerd-icons-faicon "nf-fa-exclamation")))
                 (t (let ((.error 0)
                          (.warning 0)
                          (.note 0))
                      (cl-loop
                       with warning-level = (warning-numeric-level :warning)
                       with note-level = (warning-numeric-level :debug)
                       for state being the hash-values of flymake--state
                       do (cl-loop
                           with diags = (flymake--state-diags state)
                           for diag in diags do
                           (let ((severity (flymake--lookup-type-property (flymake--diag-type diag) 'severity
                                                                          (warning-numeric-level :error))))
                             (cond ((> severity warning-level) (cl-incf .error))
                                   ((> severity note-level)    (cl-incf .warning))
                                   (t                          (cl-incf .note))))))
                      (let ((num (+ .error .warning .note)))
                        (if (> num 0)
                            (string-clean-whitespace
                             (string-join
                              (list
                               (when (> .note 0)
                                 (propertize (concat (format "%s " (nerd-icons-faicon "nf-fa-info_circle")) (number-to-string .note)) 'face 'awesome-tray-module-flymake-note))
                               (when (> .warning 0)
                                 (propertize (concat (format "%s " (nerd-icons-faicon "nf-fa-warning")) (number-to-string .warning)) 'face 'awesome-tray-module-flymake-warning))
                               (when (> .error 0)
                                 (propertize (concat (format "%s " (nerd-icons-faicon "nf-fa-times_circle")) (number-to-string .error)) 'face 'awesome-tray-module-flymake-error)))
                              " "))
                          (propertize (format "%s" (nerd-icons-faicon "nf-fa-check_circle")) 'face 'awesome-tray-green-face)
                          )))))))
            flymake-state)))))
(advice-add #'awesome-tray-module-flymake-info :override #'petmacs/awesome-tray-module-flymake-info)

(provide 'init-awesome-tray-infos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-awesome-tray-infos.el ends here
