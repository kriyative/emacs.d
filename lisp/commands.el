(defun toggle-frame-width ()
  "Toggle between narrow and wide frame layouts"
  (interactive)
  (let ((z-wid (aif (assq 'width initial-frame-alist) (cdr it) 162)))
    (if (< (frame-width) z-wid)
	(set-frame-width (selected-frame) z-wid)
      (set-frame-width (selected-frame) 81))))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(defun my-previous-window ()
  "Switch to previous window"
  (interactive)
  (other-window -1))

(defun my-next-window ()
  "Switch to next window"
  (interactive)
  (other-window 1))

(defun my-other-buffer ()
  "Replacement for bury-buffer"
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun kill-files-matching (pattern)
  "Kill all buffers whose filenames match specified regexp"
  (interactive "sRegexp: ")
  (dolist (buffer (buffer-list))
    (let ((file-name (buffer-file-name buffer)))
      (if (and file-name (string-match pattern file-name))
	  (kill-buffer buffer)))))

(defun narrow-forward-page (arg)
  (interactive "p")
  (widen)
  (forward-page arg)
  (narrow-to-page))

(defun narrow-backward-page (arg)
  (interactive "p")
  (widen)
  (backward-page (1+ (or arg 1)))
  (narrow-to-page))

(defun toggle-debug-on-error ()
  (interactive)
  (setq debug-on-error (not debug-on-error))
  (message "debug-on-error set to `%s'" debug-on-error))

(defun 4col-view ()
  (interactive)
  (n-col-view 4))

(defun 3col-view ()
  (interactive)
  (n-col-view 3))

(defun 2col-view ()
  (interactive)
  (n-col-view 2))

(defun dev-split-view ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally 85)
  (save-excursion
    (other-window 1)
    (split-window-vertically)
    (split-window-horizontally (/ (window-width) 2))))

(defun fill-vertical-panes ()
  (interactive)
  (delete-other-windows)
  (let ((pane-width 80)
        (cur (selected-window)))
    (save-excursion 
      (dotimes (i (1- (/ (/ (frame-pixel-width) (frame-char-width))
                         pane-width)))
        (split-window-horizontally pane-width)
        (other-window 1)
        (bury-buffer))
      (balance-windows))
    (select-window cur)))

(defun turn-on-line-truncation ()
  (interactive)
  (set-all-line-truncation t))

(defun other-window-send-keys (keys)
  (interactive (list (read-key-sequence "Keysequence: ")))
  (let ((window (selected-window)))
    (unwind-protect
        (save-excursion
          (other-window (or current-prefix-arg 1))
          (let ((last-kbd-macro (read-kbd-macro keys)))
            (call-last-kbd-macro)))
      (select-window window))))

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

(defun dired-open-file ()
  "In Dired, open a file using its default application."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (unless (file-directory-p file)
      (message "Opening %s..." file)
      (call-process "xdg-open" nil 0 nil file))))

(defun next-page ()
  (interactive)
  (widen)
  (forward-page)
  (narrow-to-page))

(defun prev-page ()
  (interactive)
  (widen)
  (backward-page 2)
  (narrow-to-page))

(defun rmail-mime-buffer ()
  "MIME decode the contents of the current buffer."
  (interactive)
  (let* ((data (buffer-string))
         (buf (get-buffer-create "*RMAIL*"))
         (rmail-mime-mbox-buffer rmail-view-buffer)
         (rmail-mime-view-buffer buf))
    (set-buffer buf)
    (setq buffer-undo-list t)
    (let ((inhibit-read-only t))
      ;; Decoding the message in fundamental mode for speed, only
      ;; switching to rmail-mime-mode at the end for display.  Eg
      ;; quoted-printable-decode-region gets very slow otherwise (Bug#4993).
      (fundamental-mode)
      (erase-buffer)
      (insert data)
      (rmail-mime-show t)
      (rmail-mime-mode)
      (set-buffer-modified-p nil))
    (view-buffer buf)))

(require 'ansi-color)
(defun ansi-colorize-region (&optional start end)
  "ANSI colorize a region"
  (interactive (list (mark) (point)))
  (ansi-color-apply-on-region start end))

(defun alerts:disable ()
  (interactive)
  (setq alert-hide-all-notifications t))

(defun alerts:enable ()
  (interactive)
  (setq alert-hide-all-notifications nil))

(defun alerts:clear ()
  (interactive)
  (alert-log-clear '(:message "-- clear --")))

(defun alerts:show ()
  (interactive)
  (let ((buf (get-buffer "*Alert*")))
    (if buf
        (pop-to-buffer buf)
      (message "No *Alerts* buffer"))))

(defun go-home ()
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "*scratch*"))

(defun keepass (&optional kdb-file)
  "A minimal keepass interaction mode using kpcli"
  (interactive "fFind KDB file: ")
  (let ((buf (make-comint "kpcli" "kpcli" nil
                          "--kdb" (expand-file-name kdb-file))))
    (with-current-buffer buf
      (make-variable-buffer-local 'comint-password-prompt-regexp)
      (setq comint-password-prompt-regexp
            (concat "\\(^Please provide the master password:\\|"
                    comint-password-prompt-regexp
                    "\\)")))
    (switch-to-buffer buf)))

(defun mu4e-get-inbox-maildirs ()
  (remove-if-not (lambda (x)
                   (string-match "INBOX$" x))
                 (mu4e-get-maildirs)))

(defun mu4e-ask-inbox (prompt)
  "Ask the user for a shortcut (using PROMPT) as defined in
`mu4e-maildir-shortcuts', then return the corresponding folder
name. If the special shortcut 'o' (for _o_ther) is used, or if
`mu4e-maildir-shortcuts' is not defined, let user choose from all
maildirs under `mu4e-maildir'."
  (let ((prompt (mu4e-format "%s" prompt)))
    (funcall mu4e-completing-read-function
             prompt
             (mu4e-get-inbox-maildirs))))

(defun mu4e~headers-jump-to-inbox (maildir)
  "Show the messages in maildir (user is prompted to ask what
maildir)."
  (interactive
   (let ((maildir (mu4e-ask-inbox "Jump to maildir: ")))
     (list maildir)))
  (when maildir
    (mu4e-mark-handle-when-leaving)
    (mu4e-headers-search
     (format "maildir:\"%s\"" maildir))))

(defun assoc-cdr (key alist)
  (cdr (assoc key alist)))

(defvar org-gcal-multi-account-id nil)
(defvar org-gcal-multi-accounts-fun nil)
(defvar org-gcal-multi-accounts-cursor nil)

(defun org-gcal-multi-do-next ()
  (setq org-gcal-multi-accounts-cursor (cdr org-gcal-multi-accounts-cursor))
  (when org-gcal-multi-accounts-cursor
    (org-gcal-multi-do)))

(defun org-gcal-multi-do ()
  (let* ((i (car org-gcal-multi-accounts-cursor))
         (acct (cdr i)))
    (setq org-gcal-multi-account-id (car i)
          org-gcal-client-id (assoc-cdr 'org-gcal-client-id acct)
          org-gcal-client-secret (assoc-cdr 'org-gcal-client-secret acct)
          org-gcal-file-alist (assoc-cdr 'org-gcal-file-alist acct)
          org-gcal-token-file (assoc-cdr 'org-gcal-token-file acct)
          org-gcal-token-plist nil
          browse-url-browser-function 'browse-url-default-browser)
    (message "org-gcal-multi-do: %S..." org-gcal-multi-account-id)
    (funcall org-gcal-multi-accounts-fun)))

(defun org-gcal-multi-fetch ()
  (interactive)
  (setq org-gcal-multi-accounts-cursor org-gcal-accounts
        org-gcal-multi-accounts-fun
        (lambda ()
          (org-gcal-fetch nil 'org-gcal-multi-do-next)))
  (org-gcal-multi-do))

(defun org-gcal-multi-sync ()
  (interactive)
  (setq org-gcal-multi-accounts-cursor org-gcal-accounts
        org-gcal-multi-accounts-fun
        (lambda ()
          (org-gcal-sync nil nil nil 'org-gcal-multi-do-next)))
  (org-gcal-multi-do))

(defun org-gcal-token-fetch ()
  (org-gcal-refresh-token 'org-gcal-sync t)
  (message "%S: %S,%S"
           org-gcal-multi-account-id
           org-gcal-token-plist
           (org-gcal--get-access-token)))

(defun mu4e-stop ()
  "Stop the mu4e update process"
  (interactive)
  (when (process-live-p mu4e~proc-process)
    (stop-process mu4e~proc-process))
  (setq mu4e-update-interval nil))

(defun mu4e-start ()
  "Start the mu4e update process if it's not already running"
  (interactive)
  (unless (and mu4e~proc-process
	       (process-live-p mu4e~proc-process))
    (mu4e~proc-start)
    (setq mu4e-update-interval 300)))

(defun top-mem-abusers ()
  (interactive)
  (run "top-mem-abusers"))

(defun top-pcpu-abusers ()
  (interactive)
  (run "top-pcpu-abusers"))

(defvar daily-agenda-timer (parse-relative-time "9:00 am"))
;; (decode-time daily-agenda-timer)

(require 'org)
(defun show-daily-agenda ()
  (unless (time-less-p (current-time) daily-agenda-timer)
    (setq daily-agenda-timer (time-add daily-agenda-timer
				       (seconds-to-time 86400)))
    (org-agenda-list)))

(defun set-window-width* (width pixelwisep)
  (let* ((w (selected-window))
         (delta (- width (window-width w pixelwisep))))
    (window-resize w delta t nil pixelwisep)))

(defun set-window-width (&optional width)
  (interactive "nWidth: ")
  (set-window-width* width nil))

(defun set-window-pixel-width (&optional width)
  (interactive "nWidth: ")
  (set-window-width* width t))
