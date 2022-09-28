(use-package ediff
  :config
  (set 'ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package sql
  :config
  (dolist (pv '((:prompt-regexp "^[-[:alnum:]_]*=[#>] ")
                (:prompt-cont-regexp "^[-[:alnum:]_]*[-(][#>] ")))
    (apply 'sql-set-product-feature 'postgres pv)))

(use-package json-mode :straight t)

(defun rk-java-mode-hook ()
  (setq tab-width 4))

(use-package cc-mode
  :config
  (add-hook 'java-mode-hook 'rk-java-mode-hook))

(use-package git-link :straight t)

(use-package image
  :bind
  (:map image-map
        ("w" . image-transform-fit-to-width)
        ("h" . image-transform-fit-to-height)
        ("s" . image-transform-set-scale)))

(use-package window-numbering
  :straight t
  :config
  ;; (dotimes (i 10)
  ;;   (define-key ctlx-ctlj-map
  ;;     (prin1-to-string i)
  ;;     (intern (concat "select-window-" (prin1-to-string i)))))
  (window-numbering-mode)
  (window-numbering-update))

(use-package projectile
  :straight t
  :config
  (setq projectile-keymap-prefix (kbd "C-c C-p")))

(use-package csv-mode
  :straight t
  :config
  (setq csv-align-style 'auto))

(use-package graphviz-dot-mode
  :straight t
  :config
  (setq graphviz-dot-view-command "xdot %s"))

(use-package restclient :straight t)

(use-package plantuml-mode
  :straight t
  :config
  ;; why is the default to connect to plantuml.com?
  (setq plantuml-server-url nil
        plantuml-default-exec-mode 'jar))

(use-package markdown-mode
  :straight t)
