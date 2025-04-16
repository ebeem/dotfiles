(use-package aider
  :ensure (:host github :repo "tninja/aider.el")
  :config
  (setenv "OLLAMA_API_BASE" "http://127.0.0.1:11434")
  (setq aider-args '("--no-auto-commits" "--model" "ollama_chat/qwen2.5-coder")))

(provide 'oz-ai)
;;; oz-utilities.el ends here
