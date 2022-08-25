(rk-require-packages lsp-mode
		     lsp-ui
		     treemacs
		     lsp-treemacs
                     dap-mode)

(defun rk--lsp-mode-hook ()
  (lsp-headerline-breadcrumb-mode -1))

(use-package lsp-mode
  ;; :hook ((lsp-mode . rk--lsp-mode-hook))
  :init
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-completion-enable-additional-text-edit nil)
  :config
  (setq lsp-log-io t
        lsp-print-io t))

(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))
