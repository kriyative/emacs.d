(rk-el-get-bundles ov
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

(use-package matrix-client
  :config
  (add-hook 'matrix-client-room-list-mode-hook
            'rk--matrix-client-room-list-mode-hook)
  (add-hook 'matrix-client-mode-hook 'rk--matrix-client-mode-hook)
  (setq matrix-client-save-token t
        matrix-client-save-token-file "~/.emacs.d/matrix-client.el.token"))
