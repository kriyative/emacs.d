(rk-el-get-bundles
 kriyative/git-code-review
 geiser
 json-mode
 json-snatcher)

;;;;;;;;;;;;;;;; packages ;;;;;;;;;;;;;;;;

(use-package git-code-review
  :config
  (add-hook 'clojure-mode-hook 'gcr-mode)
  (add-hook 'emacs-lisp-mode-hook 'gcr-mode)
  (add-hook 'common-lisp-mode-hook 'gcr-mode))

(use-package ediff
  :config
  (set 'ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package sql
  :config
  (dolist (pv '((:prompt-regexp "^[-[:alnum:]_]*=[#>] ")
                (:prompt-cont-regexp "^[-[:alnum:]_]*[-(][#>] ")))
    (apply 'sql-set-product-feature 'postgres pv)))

(use-package json-mode)

(defun rk-java-mode-hook ()
  (setq tab-width 4))

(use-package cc-mode
  :config
  (add-hook 'java-mode-hook 'rk-java-mode-hook))

(defun setup-geiser ()
  )

(use-package geiser
  :config
  (add-hook 'geiser-mode-hook 'setup-geiser))

(defun rk-chez ()
  (interactive)
  (run-geiser 'chez))

(defun rk-guile ()
  (interactive)
  (run-geiser 'guile))

(defun rk-racket ()
  (interactive)
  (run-geiser 'racket))

(use-package python
  :config
  (setq python-shell-interpreter "python3"))
