(rk-el-get-bundles
 hagleitn/speed-type
 ht
 iqbalansari/emacs-emojify
 (speed-type-patterns
  :url "https://gitlab.com/kriyative/speed-type-patterns.git"
  :features speed-type-patterns)
 key-chord)

(defun rk--midnight-hook ()
  (when (fboundp 'rk-org-gcal-multi-fetch-if-stale)
    (rk-org-gcal-multi-fetch-if-stale)))

(use-package midnight
  :config
  (midnight-delay-set 'midnight-delay "9:00am")
  (add-hook 'midnight-hook 'rk--midnight-hook))

(use-package ht)

(use-package emojify
  :config
  (add-hook 'mu4e-view-mode-hook 'emojify-mode))

;; (use-package key-chord)
;; (key-chord-define emacs-lisp-mode-map "df" "(defun  ()\n)\M-b\M-f\C-f")
;; (key-chord-define emacs-lisp-mode-map "up" "(use-package ")
;; (key-chord-define emacs-lisp-mode-map "((" "[")
;; (key-chord-define emacs-lisp-mode-map "ff" ffap)
;; (setq key-chord-two-keys-delay 0.08)

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
           (message "rk-aws-doc: args=%S" args)
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

