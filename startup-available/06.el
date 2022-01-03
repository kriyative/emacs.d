(rk-el-get-bundles org-caldav)

(use-package org-caldav)

(defun rk--midnight-hook ()
  (when (fboundp 'rk-org-gcal-multi-fetch-if-stale)
    (rk-org-gcal-multi-fetch-if-stale))
  (org-caldav-sync))

(use-package midnight
  :config
  (midnight-delay-set 'midnight-delay "9:00am")
  (add-hook 'midnight-hook 'rk--midnight-hook))

(when (file-exists-p diary-file)
  (diary 0))

(use-package server
  :config
  (setq server-socket-dir "~/.emacs.d/server/"
        server-name "server"
        server-use-tcp t)
  (server-force-delete)
  (server-start))

(when (fboundp 'mbsync-sync-accounts)
  (mbsync-sync-accounts))

(defun rk-aws-doc (commands)
  (interactive "sCommand: ")
  (let ((bufname (concat "*AWS-doc: " commands "*")))
    (pop-to-buffer
     (or (get-buffer bufname)
         (let ((buf (get-buffer-create bufname))
               (args (append
                      '("--color" "off"
                        "--no-cli-pager"
                        "--no-paginate")
                      (split-string commands)
                      '("help"))))
           (with-current-buffer buf
             (apply 'call-process
                    "aws"
                    nil
                    t
                    t
                    args)
             (goto-char (point-min))
             (while (re-search-forward "." nil t)
               (replace-match "")))
           buf)))))

(defun rk-countdown-timer-alert (message)
  (let ((alert-default-style 'mode-line))
    (alert message)))

(defun rk-countdown-timer (&optional seconds message)
  (interactive
   (list
    (if current-prefix-arg
        (read-number "Seconds: " 60)
      60)
    (if current-prefix-arg
        (read-string "Message: ")
      "Countdown timer alert!")))
  (run-with-timer seconds nil 'rk-countdown-timer-alert message))

(defun rk-make-window-east ()
  (interactive)
  (split-window (frame-root-window) nil 'right))

(defun rk-make-window-south ()
  (interactive)
  (split-window (frame-root-window) nil 'below))

(rk-bind-keys
 '(("RET" rk-countdown-timer)
   ("C-x 2" rk-make-window-south)
   ("C-x 3" rk-make-window-east))
 user-commands-prefix-map)

