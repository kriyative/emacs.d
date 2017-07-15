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

(defun 3col-view ()
  (interactive)
  (n-col-view 3))

(defun 2col-view ()
  (interactive)
  (n-col-view 2))

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
(defun keepass (&optional kdb-file)
  "A minimal keepass interaction mode using kpcli"
  (interactive "fFind KDB file: ")
  (switch-to-buffer
   (make-comint "kpcli" "kpcli" nil
                "--kdb" (expand-file-name kdb-file)))
  (make-variable-buffer-local 'comint-password-prompt-regexp)
  (setq comint-password-prompt-regexp
        (concat "\\(^Please provide the master password:\\|"
                comint-password-prompt-regexp
                "\\)")))
