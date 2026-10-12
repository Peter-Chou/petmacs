;; -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:
;;
;; Window configurations.
;;

;;; Code:

(use-package project-x
  :custom
  ;; auto-save project state after 30 seconds of idle time
  (project-x-auto-save-delay 30) ; nil to disable autosave
  ;; use the custom prompter that shows session labels
  (project-prompter #'project-x--project-prompt)
  ;; automatically restore the last project on startup
  (project-x-restore-last-project-on-startup nil)
  ;; not display tab bar
  (tab-bar-show nil)
  :hook
  (after-init     . project-x-mode)
  (project-x-mode . project-x-tabs-mode))

(provide 'init-workspace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-workspace.el ends here
