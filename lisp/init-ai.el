;; init-ai.el --- Initialize AI configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; AI configurations.
;;

;;; Code:

(provide 'init-ai)

(eval-when-compile
  (require 'init-const))

;; Interact with ChatGPT or other LLMs
;;
;; API Key management via authinfo (no plain-text secrets):
;;
;;   machine models.inference.ai.azure.com  login apikey password <gh_token>
;;   machine api.deepseek.com               login apikey password <ds_key>
;;   machine dashscope.aliyuncs.com         login apikey password <qwen_key>
;;   machine open.bigmodel.cn               login apikey password <zhipu_key>
;;   machine generativelanguage.googleapis.com login apikey password <gemini_key>
;;   machine api.anthropic.com              login apikey password <claude_key>
;;
(use-package gptel
  :diminish
  :functions (gptel-make-openai gptel-make-anthropic
               gptel-make-deepseek gptel-make-gemini
               gptel-make-ollama gptel-api-key-from-environment)
  :bind (("C-<f12>"   . gptel)
         ("C-M-<f12>" . gptel-menu))
  :hook (gptel-mode . gptel-highlight-mode)
  :config
  (require 'gptel)
  ;; Register backends and setup models
  ;; Securing API keys with authinfo (see `auth-sources')
  ;; format: "machine {HOST} login apikey password {token}"
  (setq gptel-model 'deepseek-v4-pro
        gptel-backend
        (gptel-make-openai "qianfan"
          :host "qianfan.baidubce.com"
          :endpoint "/v2/chat/completions"
          :stream t
          :key 'gptel-api-key
          :models '(deepseek-v4-pro)))

  ;; GLM
  (gptel-make-openai "GLM"
    :host "open.bigmodel.cn"
    :endpoint "/api/paas/v4/chat/completions"
    :stream t
    :key 'gptel-api-key
    :models '(glm-5.2 glm-5.2-flash glm-4.7 glm-4.7-flash))

  (gptel-make-deepseek "DeepSeek"
    :stream t
    :key 'gptel-api-key
    :models '(deepseek-chat deepseek-reasoner))

  ;; Qwen (Alibaba Cloud)
  (gptel-make-openai "Qwen"
    :host "dashscope.aliyuncs.com"
    :endpoint "/compatible-mode/v1/chat/completions"
    :stream t
    :key 'gptel-api-key
    :models '(qwen-plus qwen-turbo-latest qwen-max)))

;; Generate commit messages for magit
(use-package gptel-magit
  :hook (magit-mode . gptel-magit-install))

;; A native shell experience to interact with ACP agents
(when emacs/>=29p
  (use-package agent-shell
    :diminish agent-shell-ui-mode
    :commands agent-shell-insert
    :defines magit-mode-map
    :functions (magit-staged-files magit-commit-p magit-thing-at-point)
    :custom (agent-shell-display-action '(display-buffer-reuse-window))
    :bind (("<f12>"      . agent-shell)
           ("<f13>"      . agent-shell)
           ("C-c a"      . agent-shell)
           ("C-c A"      . agent-shell-new-shell)
           :map agent-shell-mode-map
           ("C-h ?"      . agent-shell-help-menu)
           ("C-<return>" . agent-shell-help-menu)
           :map magit-mode-map
           ("C-c C-g"    . my/agent-shell-magit-generate-commit)
           ("C-c C-r"    . my/agent-shell-review-magit-commit))
    :config
    (with-eval-after-load 'magit
      (defun my/agent-shell-magit-generate-commit ()
        "Generate conventional message and commit stage changes in magit."
        (interactive)
        (if (magit-staged-files)
            (agent-shell-insert
             :submit t
             :text "Commit changes with conventional message")
          (user-error "No staged changes")))

      (defun my/agent-shell-review-magit-commit ()
        "Send the commit from magit to agent-shell for reviews."
        (interactive)
        (if-let ((commit (magit-commit-p (magit-thing-at-point 'git-revision t))))
            (agent-shell-insert
             :submit t
             :text (format "Review commit: %s" commit))
          (user-error "No magit commit at point"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
