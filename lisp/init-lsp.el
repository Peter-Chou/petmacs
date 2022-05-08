;; init-lsp.el --- Setup lsp.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; @see https://emacs-lsp.github.io/lsp-mode/page/performance
(setq read-process-output-max (* 1024 1024)) ;; 1MB
(setenv "LSP_USE_PLISTS" "true")

(use-package lsp-mode
  :preface
  (defun petmacs//lsp-avy-document-symbol (all)
    (interactive)
    (let ((line 0) (col 0) (w (selected-window))
          (ccls (and (memq major-mode '(c-mode c++-mode objc-mode)) (eq c-c++-backend 'lsp-ccls)))
          (start-line (1- (line-number-at-pos (window-start))))
          (end-line (1- (line-number-at-pos (window-end))))
          ranges point0 point1
          candidates)
      (save-excursion
	(goto-char 1)
	(cl-loop for loc in
		 (lsp--send-request
                  (lsp--make-request
                   "textDocument/documentSymbol"
                   `(:textDocument ,(lsp--text-document-identifier)
                     :all ,(if all t :json-false)
                     :startLine ,start-line :endLine ,end-line)))
		 for range = (if ccls
				 loc
                               (->> loc (gethash "location") (gethash "range")))
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
      (avy-with avy-document-symbol
	(avy--process candidates
		      (avy--style-fn avy-style)))))
  (defun petmacs/lsp-avy-goto-word ()
    (interactive)
    (petmacs//lsp-avy-document-symbol t))

  (defun petmacs/lsp-avy-goto-symbol ()
    (interactive)
    (petmacs//lsp-avy-document-symbol nil))
  :diminish
  :defines (lsp-clients-python-library-directories
            lsp-rust-server)
  :commands (lsp-enable-which-key-integration
             lsp-format-buffer
             lsp-organize-imports
             lsp-install-server)
  :custom-face
  (lsp-headerline-breadcrumb-path-error-face
   ((t :underline (:style wave :color ,(face-foreground 'error))
       :inherit lsp-headerline-breadcrumb-path-face)))
  (lsp-headerline-breadcrumb-path-warning-face
   ((t :underline (:style wave :color ,(face-foreground 'warning))
       :inherit lsp-headerline-breadcrumb-path-face)))
  (lsp-headerline-breadcrumb-path-info-face
   ((t :underline (:style wave :color ,(face-foreground 'success))
       :inherit lsp-headerline-breadcrumb-path-face)))
  (lsp-headerline-breadcrumb-path-hint-face
   ((t :underline (:style wave :color ,(face-foreground 'success))
       :inherit lsp-headerline-breadcrumb-path-face)))

  (lsp-headerline-breadcrumb-symbols-error-face
   ((t :inherit lsp-headerline-breadcrumb-symbols-face
       :underline (:style wave :color ,(face-foreground 'error)))))
  (lsp-headerline-breadcrumb-symbols-warning-face
   ((t :inherit lsp-headerline-breadcrumb-symbols-face
       :underline (:style wave :color ,(face-foreground 'warning)))))
  (lsp-headerline-breadcrumb-symbols-info-face
   ((t :inherit lsp-headerline-breadcrumb-symbols-face
       :underline (:style wave :color ,(face-foreground 'success)))))
  (lsp-headerline-breadcrumb-symbols-hint-face
   ((t :inherit lsp-headerline-breadcrumb-symbols-face
       :underline (:style wave :color ,(face-foreground 'success)))))

  :hook ((prog-mode . (lambda ()
			(unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode)
			  (lsp-deferred))))
	 (lsp-mode . (lambda ()
                       ;; Integrate `which-key'
                       (lsp-enable-which-key-integration)

                       ;; Format and organize imports
                       ;; (add-hook 'before-save-hook #'lsp-format-buffer t t)
                       (add-hook 'before-save-hook #'lsp-organize-imports t t))))
  :bind (:map lsp-mode-map
         ("C-c C-d" . lsp-describe-thing-at-point)
         ([remap xref-find-definitions] . lsp-find-definition)
         ([remap xref-find-references] . lsp-find-references))
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-keep-workspace-alive nil
        lsp-signature-auto-activate nil
        lsp-modeline-code-actions-enable nil
        ;; lsp-modeline-code-actions-enable t
        lsp-modeline-diagnostics-enable nil
        ;; lsp-modeline-diagnostics-enable t
	lsp-modeline-workspace-status-enable nil
	lsp-headerline-breadcrumb-enable nil

        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        ;; lsp-enable-folding t
        lsp-enable-symbol-highlighting nil
        lsp-enable-text-document-color nil

        lsp-enable-indentation nil
	lsp-enable-on-type-formatting nil
        lsp-auto-guess-root nil
        ;; lsp-prefer-capf t

	lsp-enable-on-type-formatting nil)
  :config
  (with-no-warnings
    ;; Disable `lsp-mode' in `git-timemachine-mode'
    (defun my-lsp--init-if-visible (fn &rest args)
      (unless (bound-and-true-p git-timemachine-mode)
        (apply fn args)))
    (advice-add #'lsp--init-if-visible :around #'my-lsp--init-if-visible)

    ;; Enable `lsp-mode' in sh/bash/zsh
    (defun my-lsp-bash-check-sh-shell (&rest _)
      (and (eq major-mode 'sh-mode)
           (memq sh-shell '(sh bash zsh))))
    (advice-add #'lsp-bash-check-sh-shell :override #'my-lsp-bash-check-sh-shell)

    ;; Only display icons in GUI
    (defun my-lsp-icons-get-symbol-kind (fn &rest args)
      (when (and petmacs-icon (display-graphic-p))
	(apply fn args)))
    (advice-add #'lsp-icons-get-by-symbol-kind :around #'my-lsp-icons-get-symbol-kind)

    (defun my-lsp-icons-get-by-file-ext (fn &rest args)
      (when (and petmacs-icon (display-graphic-p))
	(apply fn args)))
    (advice-add #'lsp-icons-get-by-file-ext :around #'my-lsp-icons-get-by-file-ext)

    (defun my-lsp-icons-all-the-icons-material-icon (icon-name face fallback &optional feature)
      (if (and petmacs-icon
               (display-graphic-p)
               (functionp 'all-the-icons-material)
               (lsp-icons--enabled-for-feature feature))
          (all-the-icons-material icon-name
                                  :face face)
	(propertize fallback 'face face)))
    (advice-add #'lsp-icons-all-the-icons-material-icon
		:override #'my-lsp-icons-all-the-icons-material-icon))

  (defun lsp-update-server ()
    "Update LSP server."
    (interactive)
    ;; Equals to `C-u M-x lsp-install-server'
    (lsp-install-server t)))

(use-package lsp-ui
  :custom-face
  (lsp-ui-sideline-code-action ((t (:inherit warning))))
  :bind (("C-c u" . lsp-ui-imenu)
         :map lsp-ui-mode-map
         ("M-<f6>" . lsp-ui-hydra/body)
         ("M-RET" . lsp-ui-sideline-apply-code-actions))
  :hook (lsp-mode . lsp-ui-mode)
  :init (setq lsp-ui-sideline-show-diagnostics nil
	      lsp-ui-sideline-enable nil
	      lsp-ui-sideline-show-code-actions nil
              lsp-ui-sideline-ignore-duplicate t
	      lsp-ui-doc-delay 0.1
	      lsp-ui-doc-border (face-foreground 'font-lock-comment-face nil t)
              lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
				    ,(face-foreground 'font-lock-string-face)
				    ,(face-foreground 'font-lock-constant-face)
				    ,(face-foreground 'font-lock-variable-name-face)))
  ;; Set correct color to borders
  (defun my-lsp-ui-doc-set-border ()
    "Set the border color of lsp doc."
    (setq lsp-ui-doc-border
          (if (facep 'posframe-border)
              (face-background 'posframe-border nil t)
            (face-foreground 'shadow nil t))))
  (my-lsp-ui-doc-set-border)
  (add-hook 'after-load-theme-hook #'my-lsp-ui-doc-set-border t)
  :config
  (with-no-warnings
    ;; Display peek in child frame if possible
    ;; @see https://github.com/emacs-lsp/lsp-ui/issues/441
    (defvar lsp-ui-peek--buffer nil)
    (defun lsp-ui-peek--peek-display (fn src1 src2)
      (if (childframe-workable-p)
          (-let* ((win-width (frame-width))
                  (lsp-ui-peek-list-width (/ (frame-width) 2))
                  (string (-some--> (-zip-fill "" src1 src2)
                            (--map (lsp-ui-peek--adjust win-width it) it)
                            (-map-indexed 'lsp-ui-peek--make-line it)
                            (-concat it (lsp-ui-peek--make-footer)))))
            (setq lsp-ui-peek--buffer (get-buffer-create " *lsp-peek--buffer*"))
            (posframe-show lsp-ui-peek--buffer
                           :string (mapconcat 'identity string "")
                           :min-width (frame-width)
                           :internal-border-color (face-background 'posframe-border nil t)
                           :internal-border-width 1
                           :poshandler #'posframe-poshandler-frame-center))
        (funcall fn src1 src2)))
    (defun lsp-ui-peek--peek-destroy (fn)
      (if (childframe-workable-p)
          (progn
            (when (bufferp lsp-ui-peek--buffer)
              (posframe-hide lsp-ui-peek--buffer))
            (setq lsp-ui-peek--last-xref nil))
        (funcall fn)))
    (advice-add #'lsp-ui-peek--peek-new :around #'lsp-ui-peek--peek-display)
    (advice-add #'lsp-ui-peek--peek-hide :around #'lsp-ui-peek--peek-destroy)

    ;; Handle docs
    (defun my-lsp-ui-doc--handle-hr-lines nil
      (let (bolp next before after)
        (goto-char 1)
        (while (setq next (next-single-property-change (or next 1) 'markdown-hr))
          (when (get-text-property next 'markdown-hr)
            (goto-char next)
            (setq bolp (bolp)
                  before (char-before))
            (delete-region (point) (save-excursion (forward-visible-line 1) (point)))
            (setq after (char-after (1+ (point))))
            (insert
             (concat
              (and bolp (not (equal before ?\n)) (propertize "\n" 'face '(:height 0.5)))
              (propertize "\n" 'face '(:height 0.5))
              (propertize " "
                          ;; :align-to is added with lsp-ui-doc--fix-hr-props
                          'display '(space :height (1))
                          'lsp-ui-doc--replace-hr t
                          'face `(:background ,(face-foreground 'font-lock-comment-face)))
              ;; :align-to is added here too
              (propertize " " 'display '(space :height (1)))
              (and (not (equal after ?\n)) (propertize " \n" 'face '(:height 0.5)))))))))
    (advice-add #'lsp-ui-doc--handle-hr-lines :override #'my-lsp-ui-doc--handle-hr-lines)))

(use-package lsp-ivy
  :after lsp-mode
  :bind (:map lsp-mode-map
         ([remap xref-find-apropos] . lsp-ivy-workspace-symbol)
         ("C-s-." . lsp-ivy-global-workspace-symbol))
  :config
  (with-no-warnings
    (when (icons-displayable-p)
      (defvar lsp-ivy-symbol-kind-icons
        `(,(all-the-icons-material "find_in_page" :height 0.9 :v-adjust -0.15) ; Unknown - 0
          ,(all-the-icons-faicon "file-o" :height 0.9 :v-adjust -0.02) ; File - 1
          ,(all-the-icons-material "view_module" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-lblue) ; Module - 2
          ,(all-the-icons-material "view_module" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue) ; Namespace - 3
          ,(all-the-icons-octicon "package" :height 0.9 :v-adjust -0.15) ; Package - 4
          ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange) ; Class - 5
          ,(all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.02 :face 'all-the-icons-purple) ; Method - 6
          ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02) ; Property - 7
          ,(all-the-icons-octicon "tag" :height 0.95 :v-adjust 0 :face 'all-the-icons-lblue) ; Field - 8
          ,(all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.02 :face 'all-the-icons-lpurple) ; Constructor - 9
          ,(all-the-icons-material "storage" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange) ; Enum - 10
          ,(all-the-icons-material "share" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-lblue) ; Interface - 11
          ,(all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.02 :face 'all-the-icons-purple) ; Function - 12
          ,(all-the-icons-octicon "tag" :height 0.95 :v-adjust 0 :face 'all-the-icons-lblue) ; Variable - 13
          ,(all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.02 :face 'all-the-icons-purple) ; Constant - 14
          ,(all-the-icons-faicon "text-width" :height 0.9 :v-adjust -0.02) ; String - 15
          ,(all-the-icons-material "format_list_numbered" :height 0.95 :v-adjust -0.15) ; Number - 16
          ,(all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue) ; Boolean - 17
          ,(all-the-icons-material "view_array" :height 0.95 :v-adjust -0.15) ; Array - 18
          ,(all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-blue) ; Object - 19
          ,(all-the-icons-faicon "key" :height 0.9 :v-adjust -0.02) ; Key - 20
          ,(all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0) ; Null - 21
          ,(all-the-icons-material "format_align_right" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue) ; EnumMember - 22
          ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange) ; Struct - 23
          ,(all-the-icons-octicon "zap" :height 0.9 :v-adjust 0 :face 'all-the-icons-orange) ; Event - 24
          ,(all-the-icons-material "control_point" :height 0.9 :v-adjust -0.15) ; Operator - 25
          ,(all-the-icons-faicon "arrows" :height 0.9 :v-adjust -0.02) ; TypeParameter - 26
          ))

      (lsp-defun my-lsp-ivy--format-symbol-match
        ((sym &as &SymbolInformation :kind :location (&Location :uri))
         project-root)
        "Convert the match returned by `lsp-mode` into a candidate string."
        (let* ((sanitized-kind (if (< kind (length lsp-ivy-symbol-kind-icons)) kind 0))
               (type (elt lsp-ivy-symbol-kind-icons sanitized-kind))
               (typestr (if lsp-ivy-show-symbol-kind (format "%s " type) ""))
               (pathstr (if lsp-ivy-show-symbol-filename
                            (propertize (format " · %s" (file-relative-name (lsp--uri-to-path uri) project-root))
                                        'face font-lock-comment-face)
                          "")))
          (concat typestr (lsp-render-symbol-information sym ".") pathstr)))
      (advice-add #'lsp-ivy--format-symbol-match :override #'my-lsp-ivy--format-symbol-match))))

;; Origami integration
;; (use-package lsp-origami
;;   :after lsp-mode
;;   :hook (origami-mode . lsp-origami-mode))

(when emacs/>=25.2p
  (use-package lsp-treemacs
    :after lsp-mode
    :bind (:map lsp-mode-map
           ("C-<f8>" . lsp-treemacs-errors-list)
           ("M-<f8>" . lsp-treemacs-symbols)
           ("s-<f8>" . lsp-treemacs-java-deps-list))
    :init (lsp-treemacs-sync-mode 1)
    (setq
     lsp-treemacs-deps-position-params
     `((side . right)
       (slot . 1)
       (window-width . 32))
     lsp-treemacs-symbols-position-params
     `((side . right)
       (slot . 2)
       (window-width . 32)))
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

(when sys/macp
  (use-package lsp-sourcekit
    :init (setq lsp-sourcekit-executable
                "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp")))


(provide 'init-lsp)

;;; init-lsp.el ends here
