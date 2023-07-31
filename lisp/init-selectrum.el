(use-package selectrum
  :config
  (selectrum-mode 1))

;; (selectrum-mode -1)

(use-package selectrum-prescient
  :config
  (selectrum-prescient-mode 1)
  (prescient-persist-mode 1))

(provide 'init-selectrum)
