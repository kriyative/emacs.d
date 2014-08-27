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

(defun h2sql (&optional prompt)
  (interactive "p")
  (let ((args (append (list
                       "-cp"
                       (expand-file-name
                        "~/.m2/repository/com/h2database/h2/1.3.175/h2-1.3.175.jar")
                       "org.h2.tools.Shell")
                      (when (> prompt 1) (split-string (read-string "Args: "))))))
    (message "h2sq %S" args)
    (switch-to-buffer (apply 'make-comint "h2sql" "java" nil args))))

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

(defun get-region-or-read-terms (prompt)
  (replace-regexp-in-string "[ ]+" "+" (or (region) (read-string prompt))))

(defun google (q)
  (interactive (list (get-region-or-read-terms "Google: ")))
  (browse-url
   (concat "https://www.google.com/search?q=" q)))

(defun ddg (q)
  (interactive (list (get-region-or-read-terms "DuckDuckGo: ")))
  (browse-url
   (concat "https://duckduckgo.com/?q=" q)))

(defun mdn (q)
  (interactive (list (get-region-or-read-terms "MDN: ")))
  (google (concat "site:developer.mozilla.org " q)))

(defun wikipedia (q)
  (interactive (list (get-region-or-read-terms "Wikipedia: ")))
  (browse-url
   (concat "http://en.wikipedia.org/w/index.php?search="
           (capitalize q))))

(defun emacswiki (q)
  (interactive (list (get-region-or-read-terms "emacswiki: ")))
  (browse-url
   (concat "http://www.emacswiki.org/emacs/Search?action=index&match=" q)))
