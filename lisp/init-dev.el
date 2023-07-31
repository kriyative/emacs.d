(use-package ediff
  :config
  (set 'ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package sql
  :config
  (dolist (pv '((:prompt-regexp "^[-[:alnum:]_]*=[#>] ")
                (:prompt-cont-regexp "^[-[:alnum:]_]*[-(][#>] ")))
    (apply 'sql-set-product-feature 'postgres pv)))

(use-package json-mode :ensure t)

(use-package yaml-mode :ensure t)

(defun rk-java-mode-hook ()
  (setq tab-width 4))

(use-package cc-mode
  :config
  (add-hook 'java-mode-hook 'rk-java-mode-hook))

(use-package git-link :ensure t)

(use-package image
  :bind
  (:map image-map
        ("w" . image-transform-fit-to-width)
        ("h" . image-transform-fit-to-height)
        ("s" . image-transform-set-scale))
  :config
  (setq image-use-external-converter t))

(use-package window-numbering
  :ensure t
  :config
  (window-numbering-mode)
  (window-numbering-update))

(use-package projectile
  :ensure t
  :delight
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map))
  (:map projectile-command-map
        ("C-f" . projectile-find-file)
        ("C-d" . projectile-dired))
  (:map user-commands-prefix-map
        ("C-p" . projectile-command-map)
        ("p" . projectile-command-map))
  :config
  (projectile-mode 1)
  (define-key global-map (kbd "C-x p") nil))

(use-package csv-mode
  :ensure t
  :config
  (setq csv-align-style 'auto))

(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-view-command "xdot %s"))

(use-package restclient :ensure t)

(use-package plantuml-mode
  :ensure t
  :config
  ;; why is the default to connect to plantuml.com?
  (setq plantuml-server-url nil
        plantuml-default-exec-mode 'jar))

(defun markdown-mode-hook ()
  (setq markdown-hide-urls t)
  (visual-line-mode 1))

(use-package markdown-mode
  :ensure t
  :diminish "MD"
  :config
  (add-hook 'markdown-mode-hook 'markdown-mode-hook))

(provide 'init-dev)
