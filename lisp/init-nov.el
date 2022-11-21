(use-package esxml :straight t)

(use-package nov
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (setq nov-text-width 70
        nov-variable-pitch nil))

(provide 'init-nov)
