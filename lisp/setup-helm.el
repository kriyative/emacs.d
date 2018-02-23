(el-get-bundle helm)

(use-package helm-config
  :config
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (setq helm-command-prefix-key "C-x C-h")
  (global-set-key (kbd "C-x C-h") #'helm-command-prefix)
  (dolist (mode-map (list lisp-interaction-mode-map
                          emacs-lisp-mode-map))
    (define-key mode-map
      (kbd "C-M-i")
      'helm-lisp-completion-at-point))
  (dolist (mode-map (list clojure-mode-map
                          cider-mode-map))
    (define-key mode-map
      (kbd "C-M-i")
      'complete-symbol))
  (helm-mode 1))
