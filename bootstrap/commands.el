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

(require 'w3m)
(require 'browse-url)

(defun w3m-browse-url-other-window (url &optional new-session)
  (save-excursion
    (when (one-window-p)
      (split-window-horizontally))
    (other-window 1)
    (let ((w3m-use-tab nil))
      (w3m-browse-url url new-session))))

(defun w3m-new-buffer ()
  (interactive)
  (w3m-copy-buffer nil nil nil 'empty nil))

(defun w3m-mode-hook ()
  (define-key w3m-mode-map "\M-t" 'w3m-new-buffer))

(eval-after-load 'w3m
  '(add-hook 'w3m-mode-hook 'w3m-mode-hook))

(eval-after-load 'browse-url
  '(setq browse-url-browser-function 'w3m-browse-url-other-window))

(defun query-string-encode (s)
  (replace-regexp-in-string "[ ]+" "+" s))

(defun google (q)
  (interactive
   (list (query-string-encode (or (region) (read-string "Google: ")))))
  (browse-url
   (concat "https://www.google.com/search?q=" q)))

(defun ddg ()
  (interactive)
  (browse-url
   (concat "https://duckduckgo.com/?q="
           (query-string-encode (or (region) (read-string "DuckDuckGo: "))))))

(defun mdn (q)
  (interactive
   (list (query-string-encode (or (region) (read-string "MDN: ")))))
  (google (concat "site:developer.mozilla.org " q)))

(defun wikipedia ()
  (interactive)
  (browse-url
   (concat "http://en.wikipedia.org/w/index.php?search="
	   (query-string-encode
            (capitalize (or (region) (read-string "Wikipedia: ")))))))

(defun emacswiki (q)
  (interactive (list (get-region-or-read-terms "emacswiki: ")))
  (browse-url
   (concat "http://www.emacswiki.org/emacs/Search?action=index&match="
           (query-string-encode q))))
