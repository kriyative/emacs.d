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
  (let ((alert-default-style 'mode-line)
        (frame (selected-frame)))
    (alert message)
    (unless (frame-visible-p frame)
      (raise-frame frame))))

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

(defun rk-set-mode-line-color (color)
  (interactive (list (read-color "Mode-line color: ")))
  (set-face-background 'mode-line color))

(defun rk-jitsi (&optional url)
  (interactive "sURL: ")
  (browse-url (concat url "#config.startWithVideoMuted=true")))
