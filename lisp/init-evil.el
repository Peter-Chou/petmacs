;; -*- lexical-binding: t no-byte-compile: t -*-

(use-package evil
  :preface
  (defun petmacs//evil-visual-shift-left ()
    "evil left shift without losing selection"
    (interactive)
    (call-interactively 'evil-shift-left)
    (evil-normal-state)
    (evil-visual-restore))

  (defun petmacs//evil-visual-shift-right ()
    "evil right shift without losing selection"
    (interactive)
    (call-interactively 'evil-shift-right)
    (evil-normal-state)
    (evil-visual-restore))
  :init
  (setq evil-want-C-u-scroll t
	    evil-want-integration t
	    ;; `evil-want-C-i-jump' is set to nil to avoid `TAB' being
	    ;; overlapped in terminal mode. The GUI specific `<C-i>' is used
	    ;; instead.
	    evil-want-C-i-jump nil
	    evil-want-keybinding nil ;; use evil-collection instead
	    evil-overriding-maps nil)
  (evil-mode 1)

  ;; https://emacs.stackexchange.com/questions/46371/how-can-i-get-ret-to-follow-org-mode-links-when-using-evil-mode
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "RET") nil))
  :config
  (define-key evil-normal-state-map   (kbd "C-g") #'keyboard-quit)
  (define-key evil-motion-state-map   (kbd "C-g") #'keyboard-quit)
  (define-key evil-insert-state-map   (kbd "C-g") #'keyboard-quit)
  (define-key evil-window-map         (kbd "C-g") #'keyboard-quit)
  (define-key evil-operator-state-map (kbd "C-g") #'keyboard-quit)

  (define-key evil-normal-state-map (kbd "C-w C-w") 'ace-window)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-initial-state 'vterm-mode 'insert)

  ;; (evil-set-undo-system 'undo-tree)
  (evil-set-undo-system 'undo-fu)

  ;; treat _ as word like vim
  (with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol))

  (with-eval-after-load 'eldoc
    (eldoc-add-command #'evil-cp-insert)
    (eldoc-add-command #'evil-cp-insert-at-end-of-form)
    (eldoc-add-command #'evil-cp-insert-at-beginning-of-form)
    (eldoc-add-command #'evil-cp-append))

  (when evil-want-C-u-scroll
    (define-key evil-insert-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up))
  ;; Overload shifts so that they don't lose the selection
  (define-key evil-visual-state-map (kbd "<") 'petmacs//evil-visual-shift-left)
  (define-key evil-visual-state-map (kbd ">") 'petmacs//evil-visual-shift-right))

(use-package evil-args
  :init
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

(use-package evil-anzu
  :after anzu)

(use-package evil-escape
  :hook (after-init . evil-escape-mode)
  :init (setq evil-escape-delay 0.3))

(use-package evil-nerd-commenter
  :init
  (evil-define-key 'normal prog-mode-map
    "gc" 'evilnc-comment-or-uncomment-lines
    "gp" 'evilnc-comment-or-uncomment-paragraphs
    "gy" 'evilnc-comment-and-kill-ring-save))

(use-package evil-surround
  :init
  (global-evil-surround-mode 1))

(use-package evil-visualstar
  :commands (evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :config
  (define-key evil-visual-state-map (kbd "*")
    'evil-visualstar/begin-search-forward)
  (define-key evil-visual-state-map (kbd "#")
    'evil-visualstar/begin-search-backward))

(use-package evil-goggles
  :init
  (setq evil-goggles-pulse nil
        evil-goggles-async-duration 0.1
        evil-goggles-blocking-duration 0.05)
  :config
  (if (or vim-style-visual-feedback
          hybrid-style-visual-feedback)
      (evil-goggles-mode)
    (evil-goggles-mode -1)))

(use-package evil-indent-plus
  :init
  (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
  (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
  (define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up)
  (define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up)
  (define-key evil-inner-text-objects-map "J"
    'evil-indent-plus-i-indent-up-down)
  (define-key evil-outer-text-objects-map "J"
    'evil-indent-plus-a-indent-up-down))

(use-package evil-terminal-cursor-changer
  ;; :if (not (display-graphic-p))
  :init (setq evil-visual-state-cursor 'box
              evil-insert-state-cursor 'bar
              evil-emacs-state-cursor 'hbar))

(use-package evil-matchit
  :hook (after-init . global-evil-matchit-mode))

(use-package evil-exchange
  :init
  (let ((evil-exchange-key (kbd "gx"))
        (evil-exchange-cancel-key (kbd "gX")))
    (define-key evil-normal-state-map evil-exchange-key 'evil-exchange)
    (define-key evil-visual-state-map evil-exchange-key 'evil-exchange)
    (define-key evil-normal-state-map evil-exchange-cancel-key
      'evil-exchange-cancel)
    (define-key evil-visual-state-map evil-exchange-cancel-key
      'evil-exchange-cancel)))

(use-package evil-collection
  :init
  (setq evil-collection-outline-bind-tab-p nil
        evil-collection-want-unimpaired-p nil
        evil-collection-mode-list '(replace
                                    proced
                                    simple

                                    magit
                                    magit-section
                                    magit-todos

                                    vertico
                                    consult
                                    embark

                                    diff-hl
                                    diff-mode
                                    ediff

                                    vterm
                                    which-key

                                    lsp-ui-imenu

                                    dired
                                    ibuffer
                                    org
                                    org-present
                                    org-roam
                                    ))
  :hook (after-init . evil-collection-init)
  :config
  (defun petmacs/evil-collection-dired-setup ()
    (evil-define-key 'normal dired-mode-map (kbd "RET") 'dired-find-alternate-file)
    (evil-define-key 'normal dired-mode-map (kbd "J") 'find-file)
    (evil-define-key 'normal dired-mode-map (kbd "C-+") 'find-file))
  (advice-add #'evil-collection-dired-setup :after #'petmacs/evil-collection-dired-setup))

(use-package evil-multiedit
  :commands (evil-multiedit-default-keybinds)
  :init
  (evil-multiedit-default-keybinds))

(use-package evil-textobj-line
  :init (require 'evil-textobj-line))

(use-package evil-iedit-state
  :init
  (setq iedit-current-symbol-default t
        iedit-only-at-symbol-boundaries t
        iedit-toggle-key-default nil)
  (require 'evil-iedit-state))

(provide 'init-evil)
