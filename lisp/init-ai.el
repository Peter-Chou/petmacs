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
;; format: "machine {HOST} login apikey password {token}"
;;   machine api.deepseek.com               login api-key password <ds_key>
;;   machine api.anthropic.com              login api-key password <claude_key>
;;
(use-package gptel
  :diminish
  :functions (gptel-make-openai gptel-make-deepseek
               gptel-make-anthropic gptel-make-gemini)
  :bind (("C-<f12>"   . gptel)
         ("C-M-<f12>" . gptel-menu))
  :hook (gptel-mode . gptel-highlight-mode)
  :config
  (require 'gptel)
  ;; (setq gptel-backend (gptel-make-gemini "Gemini"
  ;;                       :key (string-trim
  ;;                             (shell-command-to-string "$SHELL --login -c 'echo $GEMINI_API_KEY'"))
  ;;                       :stream t))

  (setq gptel-model 'deepseek-v4-pro
        gptel-backend
        (gptel-make-openai "qianfan"
          :host "qianfan.baidubce.com"
          :endpoint "/v2/chat/completions"
          :stream t
          :key 'gptel-api-key
          :models '(deepseek-v4-pro)))

  ;; DeepSeek
  (gptel-make-deepseek "DeepSeek"
    :stream t
    :key 'gptel-api-key)

  ;; GLM
  (gptel-make-openai "GLM"
    :host "open.bigmodel.cn"
    :endpoint "/api/paas/v4/chat/completions"
    :stream t
    :key 'gptel-api-key
    :models '(glm-5.2 glm-5.2-flash glm-4.7 glm-4.7-flash))

  ;; Qwen (Alibaba Cloud)
  (gptel-make-openai "Qwen"
    :host "dashscope.aliyuncs.com"
    :endpoint "/compatible-mode/v1/chat/completions"
    :stream t
    :key 'gptel-api-key
    :models '(qwen-plus qwen-turbo qwen-max))
  )

;; ;; Generate commit messages for magit
;; (when emacs/>=30p
;;   (use-package gptel-magit
;;     :vc (:url "https://github.com/roife/gptel-magit" :rev :newest)
;;     :hook (magit-mode . gptel-magit-install)))

;; A native shell experience to interact with ACP agents
(when emacs/>=29p
  (use-package agent-shell
    :diminish agent-shell-ui-mode
    :commands (agent-shell-insert)
    :custom (agent-shell-display-action '(display-buffer-reuse-window))
    :bind (("<f12>"      . agent-shell)
           ("<f13>"      . agent-shell)
           ("C-c a"      . agent-shell)
           ("C-c A"      . agent-shell-new-shell)
           :map agent-shell-mode-map
           ("RET" . newline)
           ("M-RET" . shell-maker-submit)
           ("C-c C-c" . shell-maker-submit)
           ("C-c C-k" . agent-shell-interrupt)
           ("C-h ?"      . agent-shell-help-menu)
           ("C-<return>" . agent-shell-help-menu))
    :init
    (require 'agent-shell)

    (setq agent-shell-qwen-environment (agent-shell-make-environment-variables
                                        "OPENAI_BASE_URL" "https://qianfan.baidubce.com/v2"
                                        "OPENAI_MODEL" "kimi-k2.6"))
    (setq agent-shell-qwen-authentication
          (agent-shell-qwen-make-authentication
           :openai-api-key (string-trim
                            (shell-command-to-string "$SHELL --login -c 'echo $ANTHROPIC_API_KEY'"))))

    :config
    ;; Evil state-specific RET behavior: insert mode = newline, normal mode = send
    (evil-define-key 'insert agent-shell-mode-map (kbd "RET") #'newline)
    (evil-define-key 'normal agent-shell-mode-map (kbd "RET") #'comint-send-input)

    ;; Configure *agent-shell-diff* buffers to start in Emacs state
    (add-hook 'diff-mode-hook
	          (lambda ()
	            (when (string-match-p "\\*agent-shell-diff\\*" (buffer-name))
		          (evil-emacs-state))))))

(provide 'init-ai)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
