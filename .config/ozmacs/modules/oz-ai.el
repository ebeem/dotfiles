(use-package gptel
  :ensure t
  :init
  (setq gptel-backend
   (gptel-make-ollama "Ollama"
	:host "localhost:11434"
	:stream t
	:models '("aeline/phil:8b"))))
	;; :models '(qwen3-coder-next:latest))))

(use-package agent-shell
    :ensure t
    :config
	(setq agent-shell-anthropic-authentication
		  (agent-shell-anthropic-make-authentication :login t)
		  agent-shell-google-authentication
		  (agent-shell-google-make-authentication :login t)))

(provide 'oz-ai)
;;; oz-ai.el ends here
