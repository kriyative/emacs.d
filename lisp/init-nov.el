(use-package esxml :straight t)

(use-package nov
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(provide 'init-nov)
