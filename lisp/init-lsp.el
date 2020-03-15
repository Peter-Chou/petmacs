;; init-lsp.el --- Setup lsp.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(use-package lsp-mode
  :diminish
  ;; :pin melpa-stable
  :preface
  (defun petmacs//lsp-avy-document-symbol (all)
    (interactive)
    (let ((line 0) (col 0) (w (selected-window))
	  (ccls (memq major-mode '(c-mode c++-mode objc-mode)))
	  (start-line (1- (line-number-at-pos (window-start))))
	  (end-line (1- (line-number-at-pos (window-end))))
	  ranges point0 point1
	  candidates)
      (save-excursion
	(goto-char 1)
	(cl-loop for loc in
		 (lsp--send-request (lsp--make-request
				     "textDocument/documentSymbol"
				     `(:textDocument ,(lsp--text-document-identifier)
						     :all ,(if all t :json-false)
						     :startLine ,start-line :endLine ,end-line)))
		 for range = (if ccls loc (->> loc (gethash "location") (gethash "range")))
		 for range_start = (gethash "start" range)
		 for range_end = (gethash "end" range)
		 for l0 = (gethash "line" range_start)
		 for c0 = (gethash "character" range_start)
		 for l1 = (gethash "line" range_end)
		 for c1 = (gethash "character" range_end)
		 while (<= l0 end-line)
		 when (>= l0 start-line)
		 do
		 (forward-line (- l0 line))
		 (forward-char c0)
		 (setq point0 (point))
		 (forward-line (- l1 l0))
		 (forward-char c1)
		 (setq point1 (point))
		 (setq line l1 col c1)
		 (push `((,point0 . ,point1) . ,w) candidates)))
      ;; (require 'avy)
      (avy-with avy-document-symbol
	(avy--process candidates
		      (avy--style-fn avy-style)))))

  (defun petmacs/lsp-avy-goto-word ()
    (interactive)
    (petmacs//lsp-avy-document-symbol t))


  (defun spacemacs/lsp-avy-goto-symbol ()
    (interactive)
    (petmacs//lsp-avy-document-symbol nil))

  (defun petmacs/lsp-ui-doc-func ()
    "Toggle the function signature in the lsp-ui-doc overlay"
    (interactive)
    (setq lsp-ui-doc-include-signature (not lsp-ui-doc-include-signature)))

  (defun petmacs/lsp-ui-sideline-symb ()
    "Toggle the symbol in the lsp-ui-sideline overlay.
(generally redundant in C modes)"
     (interactive)
     (setq lsp-ui-sideline-show-symbol (not lsp-ui-sideline-show-symbol)))

  (defun petmacs/lsp-ui-sideline-ignore-duplicate ()
    "Toggle ignore duplicates for lsp-ui-sideline overlay"
    (interactive)
    (setq lsp-ui-sideline-ignore-duplicate (not lsp-ui-sideline-ignore-duplicate)))

  (defun petmacs/lsp-find-definition-other-window ()
    (interactive)
    (let ((pop-up-windows t))
      (pop-to-buffer (current-buffer) t))
    (xref-find-definitions))

  (defun petmacs/lsp-find-declaration-other-window ()
    (interactive)
    (let ((pop-up-windows t))
      (pop-to-buffer (current-buffer) t))
    (lsp-find-declaration))

  (defun petmacs/lsp-find-references-other-window ()
    (interactive)
    (let ((pop-up-windows t))
      (pop-to-buffer (current-buffer) t))
    (lsp-find-references))

  (defun petmacs/lsp-find-implementation-other-window ()
    (interactive)
    (let ((pop-up-windows t))
      (pop-to-buffer (current-buffer) t))
    (lsp-find-implementation))

  (defun petmacs/lsp-find-type-definition-other-window ()
    (interactive)
    (let ((pop-up-windows t))
      (pop-to-buffer (current-buffer) t))
    (lsp-find-type-definition))
  :hook (lsp-mode . (lambda ()
                       ;; Integrate `which-key'
                       (lsp-enable-which-key-integration)))
  :bind (:map lsp-mode-map
            ("C-c C-d" . lsp-describe-thing-at-point)
            ([remap xref-find-definitions] . lsp-find-definition)
            ([remap xref-find-references] . lsp-find-references))
  :init
  (setq lsp-auto-guess-root nil		;; Detect project root
        lsp-keep-workspace-alive nil    ;; Auto-kill LSP server
	lsp-prefer-flymake nil		;; Use lsp-ui and flycheck
	flymake-fringe-indicator-position 'right-fringe)
  :config
     ;; Configure LSP clients
     (use-package lsp-clients
       :ensure nil
       :hook (go-mode . (lambda ()
                          "Format and add/delete imports."
                          (add-hook 'before-save-hook #'lsp-format-buffer t t)
                          (add-hook 'before-save-hook #'lsp-organize-imports t t)))))

(use-package lsp-ui
  ;; :pin melpa-stable
  :functions my-lsp-ui-imenu-hide-mode-line
  :commands lsp-ui-doc-hide
  :custom-face
  (lsp-ui-sideline-code-action ((t (:inherit warning))))
  :bind (("C-c u" . lsp-ui-imenu)
            :map lsp-ui-mode-map
            ("M-<f6>" . lsp-ui-hydra/body))
  :hook ((lsp-mode . lsp-ui-mode)
	 (lsp-ui-imenu-mode . (lambda ()
			       (display-line-numbers-mode -1)
			       (hl-line-mode -1))))
  :init
  (setq lsp-ui-doc-enable t
	lsp-ui-doc-use-webkit nil
	lsp-ui-doc-delay 0.2
	lsp-ui-doc-include-signature t
	lsp-ui-doc-position 'top  ;; or at-point
	lsp-ui-doc-border (face-foreground 'default)
        lsp-eldoc-enable-hover nil ; Disable eldoc displays in minibuffer
	;; lsp-ui-flycheck-enable nil  ;; disable flycheck
	;; lsp-ui-peek-enable t

	lsp-ui-sideline-enable t
	lsp-ui-sideline-show-hover nil
	lsp-ui-sideline-show-diagnostics nil
	lsp-ui-sideline-ignore-duplicate t

	lsp-ui-imenu-enable t
        lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                              ,(face-foreground 'font-lock-string-face)
                              ,(face-foreground 'font-lock-constant-face)
                              ,(face-foreground 'font-lock-variable-name-face)))

  (evil-define-key 'normal lsp-ui-imenu-mode-map (kbd "q") 'lsp-ui-imenu--kill)
  (evil-define-key 'normal lsp-ui-imenu-mode-map (kbd "J") 'lsp-ui-imenu--next-kind)
  (evil-define-key 'normal lsp-ui-imenu-mode-map (kbd "K") 'lsp-ui-imenu--prev-kind)
  (evil-define-key 'normal lsp-ui-imenu-mode-map (kbd "<return>") 'lsp-ui-imenu--visit)
  (evil-define-key 'normal lsp-ui-imenu-mode-map (kbd "d") 'lsp-ui-imenu--view)
  :config
  (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

  ;; Reset `lsp-ui-doc-background' after loading theme
  (add-hook 'after-load-theme-hook
	    (lambda ()
	      (setq lsp-ui-doc-border (face-foreground 'default))
	      (set-face-background 'lsp-ui-doc-background
				   (face-background 'tooltip)))))

;; Completion
(use-package company-lsp
    :init (setq company-lsp-cache-candidates 'auto)
    :config
    ;; WORKAROUND:Fix tons of unrelated completion candidates shown
    ;; when a candidate is fulfilled
    ;; @see https://github.com/emacs-lsp/lsp-python-ms/issues/79
    (add-to-list 'company-lsp-filter-candidates '(mspyls))

    (with-no-warnings
    (defun my-company-lsp--on-completion (response prefix)
	"Handle completion RESPONSE.
PREFIX is a string of the prefix when the completion is requested.
Return a list of strings as the completion candidates."
	(let* ((incomplete (and (hash-table-p response) (gethash "isIncomplete" response)))
	    (items (cond ((hash-table-p response) (gethash "items" response))
			    ((sequencep response) response)))
	    (candidates (mapcar (lambda (item)
				    (company-lsp--make-candidate item prefix))
				(lsp--sort-completions items)))
	    (server-id (lsp--client-server-id (lsp--workspace-client lsp--cur-workspace)))
	    (should-filter (or (eq company-lsp-cache-candidates 'auto)
				(and (null company-lsp-cache-candidates)
				    (company-lsp--get-config company-lsp-filter-candidates server-id)))))
	(when (null company-lsp--completion-cache)
	    (add-hook 'company-completion-cancelled-hook #'company-lsp--cleanup-cache nil t)
	    (add-hook 'company-completion-finished-hook #'company-lsp--cleanup-cache nil t))
	(when (eq company-lsp-cache-candidates 'auto)
	    ;; Only cache candidates on auto mode. If it's t company caches the
	    ;; candidates for us.
	    (company-lsp--cache-put prefix (company-lsp--cache-item-new candidates incomplete)))
	(if should-filter
	    (company-lsp--filter-candidates candidates prefix)
	    candidates)))
    (advice-add #'company-lsp--on-completion :override #'my-company-lsp--on-completion)))

;; Ivy integration
(use-package lsp-ivy
    :after lsp-mode
    :bind (:map lsp-mode-map
	([remap xref-find-apropos] . lsp-ivy-workspace-symbol)
	("C-s-." . lsp-ivy-global-workspace-symbol)))

;; Origami integration
(use-package lsp-origami
  :after lsp-mode
  :hook (origami-mode . lsp-origami-mode))

;; Debug
;; python: pip install "ptvsd>=4.2" 
;; C++: build lldb from https://github.com/llvm-mirror/lldb/tree/master/tools/lldb-vscode
(use-package dap-mode
  :diminish
  :bind (:map lsp-mode-map
         ("<f5>" . dap-debug)
         ("M-<f5>" . dap-hydra))
  :hook ((after-init . dap-mode)
         (dap-mode . dap-ui-mode)
         (dap-session-created . (lambda (_args) (dap-hydra)))
         (dap-stopped . (lambda (_args) (dap-hydra)))

         (python-mode . (lambda () (require 'dap-python)))
         ;; (ruby-mode . (lambda () (require 'dap-ruby)))
         (go-mode . (lambda () (require 'dap-go)))
         (java-mode . (lambda () (require 'dap-java)))
         ((c-mode c++-mode objc-mode swift-mode) . (lambda () (require 'dap-lldb)))
         ;; (php-mode . (lambda () (require 'dap-php)))
         ;; (elixir-mode . (lambda () (require 'dap-elixir)))
         ;; ((js-mode js2-mode) . (lambda () (require 'dap-chrome)))
         (powershell-mode . (lambda () (require 'dap-pwsh)))))

;; `lsp-mode' and `treemacs' integration
(when emacs/>=25.2p
  (use-package lsp-treemacs
    :after lsp-mode
    :bind (:map lsp-mode-map
           ("C-<f8>" . lsp-treemacs-errors-list)
           ("M-<f8>" . lsp-treemacs-symbols)
           ("s-<f8>" . lsp-treemacs-java-deps-list))
    :config
    (with-eval-after-load 'ace-window
      (when (boundp 'aw-ignored-buffers)
        (push 'lsp-treemacs-symbols-mode aw-ignored-buffers)
        (push 'lsp-treemacs-java-deps-mode aw-ignored-buffers)))

    (with-no-warnings
    (when (require 'all-the-icons nil t)
        (treemacs-create-theme "petmacs-colors"
          :extends "doom-colors"
          :config
          (progn
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "repo" :height 1.0 :v-adjust -0.1 :face 'all-the-icons-blue))
             :extensions (root))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
             :extensions (boolean-data))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "settings_input_component" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-orange))
             :extensions (class))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "palette" :height 0.95 :v-adjust -0.15))
             :extensions (color-palette))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "square-o" :height 0.95 :v-adjust -0.05))
             :extensions (constant))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "file-text-o" :height 0.95 :v-adjust -0.05))
             :extensions (document))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "storage" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-orange))
             :extensions (enumerator))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "format_align_right" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
             :extensions (enumitem))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "bolt" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-orange))
             :extensions (event))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
             :extensions (field))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "search" :height 0.95 :v-adjust -0.05))
             :extensions (indexer))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "filter_center_focus" :height 0.95 :v-adjust -0.15))
             :extensions (intellisense-keyword))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "share" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
             :extensions (interface))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
             :extensions (localvariable))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-purple))
             :extensions (method))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "view_module" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
             :extensions (namespace))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "format_list_numbered" :height 0.95 :v-adjust -0.15))
             :extensions (numeric))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "control_point" :height 0.95 :v-adjust -0.2))
             :extensions (operator))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.05))
             :extensions (property))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "format_align_center" :height 0.95 :v-adjust -0.15))
             :extensions (snippet))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "text-width" :height 0.9 :v-adjust -0.05))
             :extensions (string))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange))
             :extensions (structure))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "format_align_center" :height 0.95 :v-adjust -0.15))
             :extensions (template))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "chevron-right" :height 0.75 :v-adjust 0.1 :face 'font-lock-doc-face))
             :extensions (collapsed) :fallback "+")
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "chevron-down" :height 0.75 :v-adjust 0.1 :face 'font-lock-doc-face))
             :extensions (expanded) :fallback "-")
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-binary" :height 0.9  :v-adjust 0.0 :face 'font-lock-doc-face))
             :extensions (classfile))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-blue))
             :extensions (default-folder-opened))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-blue))
             :extensions (default-folder))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-green))
             :extensions (default-root-folder-opened))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-green))
             :extensions (default-root-folder))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-binary" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
             :extensions ("class"))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-zip" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
             :extensions (file-type-jar))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
             :extensions (folder-open))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
             :extensions (folder))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-orange))
             :extensions (folder-type-component-opened))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-orange))
             :extensions (folder-type-component))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-green))
             :extensions (folder-type-library-opened))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-green))
             :extensions (folder-type-library))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-pink))
             :extensions (folder-type-maven-opened))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-pink))
             :extensions (folder-type-maven))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'font-lock-type-face))
             :extensions (folder-type-package-opened))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'font-lock-type-face))
             :extensions (folder-type-package))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "plus" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
             :extensions (icon-create))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "list" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
             :extensions (icon-flat))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "share" :height 0.95 :v-adjust -0.2 :face 'all-the-icons-lblue))
             :extensions (icon-hierarchical))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "link" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
             :extensions (icon-link))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "refresh" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
             :extensions (icon-refresh))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "chain-broken" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
             :extensions (icon-unlink))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-alltheicon "java" :height 1.0 :v-adjust 0.0 :face 'all-the-icons-orange))
             :extensions (jar))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "book" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-green))
             :extensions (library))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "folder-open" :face 'all-the-icons-lblue))
             :extensions (packagefolder-open))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
             :extensions (packagefolder))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "archive" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
             :extensions (package))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "repo" :height 1.0 :v-adjust -0.1 :face 'all-the-icons-blue))
             :extensions (java-project))))

        (setq lsp-treemacs-theme "petmacs-colors")))))

;; Enable LSP in org babel
;; https://github.com/emacs-lsp/lsp-mode/issues/377
(cl-defmacro lsp-org-babel-enable (lang)
  "Support LANG in org source code block."
  (cl-check-type lang stringp)
  (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
         (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
    `(progn
       (defun ,intern-pre (info)
         (let ((file-name (->> info caddr (alist-get :file))))
           (unless file-name
             (user-error "LSP:: specify `:file' property to enable"))

           (setq buffer-file-name file-name)
	   (lsp-deferred)))
       (put ',intern-pre 'function-documentation
            "Enable lsp in the buffer of org source block (%s).")

       (if (fboundp ',edit-pre)
           (advice-add ',edit-pre :after ',intern-pre)
         (progn
           (defun ,edit-pre (info)
             (,intern-pre info))
           (put ',edit-pre 'function-documentation
                (format "Prepare local buffer environment for org source block (%s)."
                        (upcase ,lang))))))))

(defvar org-babel-lang-list
  '("go" "python" "ipython" "ruby" "js" "css" "sass" "C" "rust" "java"))
(add-to-list 'org-babel-lang-list (if emacs/>=26p "shell" "sh"))
(dolist (lang org-babel-lang-list)
  (eval `(lsp-org-babel-enable ,lang)))

(provide 'init-lsp)

;;; init-lsp.el ends here
