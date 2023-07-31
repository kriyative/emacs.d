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

(use-package w3m
  :ensure t
  :config
  (setq browse-url-browser-function 'rk-url-browser-function)
  :bind (("\M-t" . w3m-copy-buffer)))

(provide 'init-w3m)
