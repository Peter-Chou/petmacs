;; -*- lexical-binding: t no-byte-compile: t -*-

(use-package tabspaces
  ;; :hook (after-init . tabspaces-mode)
  :init
  (setq tabspaces-use-filtered-buffers-as-default t
        tabspaces-default-tab "Default"
        tabspaces-remove-to-default t
        tabspaces-include-buffers '("*scratch*" "*Messages*")
        ;; sessions
        tabspaces-session t
        tabspaces-session-auto-restore t)

  ;; Filter Buffers for Consult-Buffer
  (with-eval-after-load 'consult
    ;; hide full buffer list (still available with "b" prefix)
    (consult-customize consult--source-buffer :hidden t :default nil)
    ;; set consult-workspace buffer list
    (defvar consult--source-workspace
      (list :name     "Workspace Buffer"
            :narrow   ?w
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda () (consult--buffer-query
                             :predicate #'tabspaces--local-buffer-p
                             :sort 'visibility
                             :as #'buffer-name)))
      "Set workspace buffer list for consult-buffer.")
    (add-to-list 'consult-buffer-sources 'consult--source-workspace)))

(provide 'init-workspace)
