(rk-require-packages lsp-java)

(use-package lsp-java
  :config
  (add-hook 'java-mode-hook #'lsp))

(use-package dap-java :ensure nil)
