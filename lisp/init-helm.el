(use-package helm
  :ensure t
  :config
  (setq helm-command-prefix-key "\C-c\C-h")
  (global-set-key (kbd "C-c C-h") #'helm-command-prefix)
  (helm-mode 1)
  :bind
  (("\C-xb"   . helm-mini)
   ;; ("<tab>"   . helm-execute-persistent-action)
   ;; ("C-i"     . helm-execute-persistent-action)
   ("M-w"     . helm-copy-selection)
   ("M-x"     . helm-M-x)
   ("C-x r b" . helm-filtered-bookmarks)
   ("C-x C-f" . helm-find-files)))

(use-package helm-ag
  :ensure t
  :after ag
  :bind
  (("C-c G g" . helm-ag-project-root)
   ("C-c G a" . helm-ag)))

(use-package helm-cider :ensure t)

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

(provide 'init-helm)
