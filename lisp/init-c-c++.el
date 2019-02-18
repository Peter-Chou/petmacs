
;; C/C++/Objective-C support
(use-package ccls
  :defines projectile-project-root-files-top-down-recurring
  :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda ()
                                                   (require 'ccls)
                                                   (lsp)))
  :init
  (setq ccls-executable (file-truename "~/ccls/Release/ccls"))
  (setq ccls-initialization-options `(:cacheDirectory ,(expand-file-name "~/.ccls-cache")))

  :config
  (with-eval-after-load 'projectile
    (setq projectile-project-root-files-top-down-recurring
	  (append '("compile_commands.json"
		    ".ccls")
		  projectile-project-root-files-top-down-recurring))))

(provide 'init-c-c++)
