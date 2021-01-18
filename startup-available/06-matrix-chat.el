(rk-el-get-bundles ov
                   ht
                   rainbow-identifiers
                   frame-purpose
                   alphapapa/matrix-client.el)

;; code mostly adapted from matrix-client-frame-sidebar-open-room-frame
(defun rk--matrix-client-open-room-window ()
  "Open the selected roomm in a window"
  (interactive)
  (let ((buffer (or (get-text-property (+ 7 (line-beginning-position)) 'buffer)
                    (oref* (tabulated-list-get-id) client-data buffer))))
    (when buffer
      (pop-to-buffer buffer))))

(defun rk--matrix-client-room-list-mode-hook ()
  (local-set-key (kbd "<return>") 'rk--matrix-client-open-room-window))

(defun rk--matrix-client-mode-hook ()
  (local-set-key (kbd "C-<return>") 'ffap))

(defun rk--matrix-client-notify-hook (event-type data rest)
  (notifications-notify :title "emacs-matrix"
                        :body (alist-get 'body (alist-get 'content data))
                        :app-name "Emacs"))

(use-package matrix-client
  :config
  (add-hook 'matrix-client-room-list-mode-hook
            'rk--matrix-client-room-list-mode-hook)
  (add-hook 'matrix-client-mode-hook 'rk--matrix-client-mode-hook)
  (add-hook 'matrix-client-notify-hook 'rk--matrix-client-notify-hook)
  (setq matrix-client-save-token t
        matrix-client-save-token-file "~/.emacs.d/matrix-client.el.token"
        matrix-client-room-send-as-org-by-default t))
