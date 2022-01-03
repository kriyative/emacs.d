(rk-require-packages lsp-mode
		     lsp-ui
		     treemacs
		     lsp-treemacs
		     lsp-java)

(use-package lsp-mode
  :hook
  ((lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-completion-enable-additional-text-edit nil))

(use-package treemacs)

(use-package lsp-java
  :config
  (add-hook 'java-mode-hook #'lsp))
