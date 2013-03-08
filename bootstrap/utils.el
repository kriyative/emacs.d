(defun psql (&optional prompt)
  (interactive "p")
  (switch-to-buffer
   (apply 'make-comint "psql" "psql" nil
	  (when (> prompt 1) (split-string (read-string "Args: "))))))

(defun mysql (&optional prompt)
  (interactive "p")
  (switch-to-buffer
   (apply 'make-comint "mysql5" "mysql5" nil
	  (when (> prompt 1) (split-string (read-string "Args: "))))))


(defun sysinfo ()
  (interactive)
  (message "%s@%s %s"
	   (user-login-name)
	   (system-name)
	   (replace-regexp-in-string "[\n]+$"
				     ""
				     (shell-command-to-string "uptime"))))

(defun run (command)
  (interactive "sRun program: ")
  (destructuring-bind (program &rest args)
      (split-string command " ")
    (let* ((buf-name (concat "*run:" command "*"))
           (buf-proc (get-buffer-process buf-name)))
      (if buf-proc
          (message "Process is already running.")
        (let ((buf (get-buffer buf-name)))
          (when buf (kill-buffer buf))
          (let ((name (file-name-nondirectory program))
                (buf (get-buffer-create buf-name)))
            (switch-to-buffer (apply 'make-comint-in-buffer name buf program nil args))
            (run-hooks (intern-soft (concat "comint-" name "-hook")))))))))

(defun ssh (server command &optional buffer-name-prefix)
  (interactive
   (list (read-string "Destination: " nil 'my-history)
	 (read-string "Command: " nil 'my-history)))
  (let ((buf-name (concat (or buffer-name-prefix
			      (concat "ssh@" server " " command)))))
    (apply 'make-comint
	   buf-name "ssh" nil
	   server
	   (split-string command " "))
    (pop-to-buffer (concat "*" buf-name "*"))
    (setq truncate-lines t)))

