(my-el-get-bundles
 helm
 helm-ag
 helm-cider)

(use-package helm-config
  :config
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "M-w") 'helm-copy-selection)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (setq helm-command-prefix-key "\C-c\C-h")
  (global-set-key (kbd "C-c C-h") #'helm-command-prefix)
  (dolist (mode-map (list lisp-interaction-mode-map emacs-lisp-mode-map))
    (define-key mode-map (kbd "C-M-i") 'helm-lisp-completion-at-point))
  (dolist (mode-map (list clojure-mode-map cider-mode-map))
    (define-key mode-map (kbd "C-M-i") 'complete-symbol))
  (helm-mode 1)
  (when (boundp 'projectile-completion-system)
    (setq projectile-completion-system 'helm))
  :bind
  (("\C-xb" . helm-mini)))

(use-package helm-ag
  :bind
  (("\C-cg" . helm-ag-project-root)
   ("\C-cG" . helm-ag)))

(use-package helm-cider)

(use-package helm-projectile
  :bind
  (:map user-commands-prefix-map
        ("\C-x\C-f" . helm-projectile-find-file)))

(defun helm-copy-selection (arg)
  (interactive "P")
  (with-helm-alive-p
    (helm-run-after-exit
     (lambda (sel)
       (let ((fpath (concat helm-ff-default-directory sel)))
         (kill-new fpath)
         (prog1 nil
           (message "Saved to clipboard: %s" fpath)
           (sit-for 1))))
     (format "%s" (helm-get-selection nil t)))))
