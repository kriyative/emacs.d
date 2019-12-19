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
  (helm-mode 1)
  :bind
  (("\C-xb" . helm-mini)))

(use-package helm-ag
  :bind
  (("\C-cg" . helm-ag-project-root)
   ("\C-cG" . helm-ag)))

(use-package helm-cider)

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
