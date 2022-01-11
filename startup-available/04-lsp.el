(rk-require-packages lsp-mode
		     lsp-ui
		     treemacs
		     lsp-treemacs
                     dap-mode)

(defun rk--lsp-mode-hook ()
  (lsp-headerline--disable-breadcrumb))

(use-package lsp-mode
  :hook
  ((lsp-mode . rk--lsp-mode-hook))
  :config
  (setq lsp-completion-enable-additional-text-edit nil))

(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))
