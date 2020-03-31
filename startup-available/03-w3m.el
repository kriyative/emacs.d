(rk-el-get-bundles emacs-w3m)

(defun w3m-browse-url-other-window (url &optional new-session)
  (save-excursion
    (when (one-window-p)
      (split-window-horizontally))
    (other-window 1)
    (let ((w3m-use-tab nil))
      (w3m-browse-url url new-session))))

(defun rk-url-browser-function (&rest args)
  (apply (if current-prefix-arg
             'browse-url-default-browser
           'w3m-browse-url-other-window)
         args))

(defun w3m-mode-hook ()
  (define-key w3m-mode-map "\M-t" 'w3m-copy-buffer))

(use-package w3m
  :config
  (add-hook 'w3m-mode-hook 'w3m-mode-hook)
  (setq browse-url-browser-function 'rk-url-browser-function))
