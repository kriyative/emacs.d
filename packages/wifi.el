(defcustom wifi-status-function
  (cond ((eq system-type 'gnu/linux)
         #'wifi-linux-iwconfig))
  "Function for getting wifi status information.
The function has to return an alist of conversion definitions.
Its cons cells are of the form

    (CONVERSION . REPLACEMENT-TEXT)

CONVERSION is the character code of a \"conversion specification\"
introduced by a `%' character in a control string."
  :type '(choice (const nil) function)
  :group 'wifi)

(defcustom wifi-echo-area-format
  "ESSID %E Link Quality %S"
  "Control string formatting the string to display in the echo area.
Ordinary characters in the control string are printed as-is, while
conversion specifications introduced by a `%' character in the control
string are substituted as defined by the current value of the variable
`wifi-status-function'.  Here are the ones generally available:
%E Current ESSID
%S Current Link Quality percentage"
  :type '(choice string (const nil))
  :group 'wifi)

(defcustom wifi-mode-line-format
  "[%E %S]"
  "Control string formatting the string to display in the mode line.
Ordinary characters in the control string are printed as-is, while
conversion specifications introduced by a `%' character in the control
string are substituted as defined by the current value of the variable
`wifi-status-function'.  Here are the ones generally available:
%E Current ESSID
%S Current Link Quality percentage"
  :type '(choice string (const nil))
  :group 'wifi)

(defvar wifi-mode-line-string nil
  "String to display in the mode line.")

(defcustom wifi-update-interval 60
  "Seconds after which the battery status will be updated."
  :type 'integer
  :group 'wifi)

(defvar wifi-update-timer nil
  "Interval timer object.")

(defun dbm-to-signal (dbm)
  (when dbm
    (let* ((dbm-floor 20.0)
           (dbm-range (- 90.0 dbm-floor)))
      (min 1 (- 1 (/ (- (abs dbm) dbm-floor) dbm-range))))))

(defun wifi-format (format alist)
  "Substitute %-sequences in FORMAT."
  (replace-regexp-in-string
   "%."
   (lambda (str)
     (let ((char (aref str 1)))
       (if (eq char ?%) "%"
	 (or (cdr (assoc char alist)) ""))))
   format t t))

(defun format-wifi-stats ()
  (with-temp-buffer
    (call-process "iwconfig" nil t nil)
    (goto-char (point-min))
    (let* ((essid (and (re-search-forward "ESSID:\"\\([^\"]*\\)\"" nil t)
                       (match-string 1)))
           (lq (and (re-search-forward "Link Quality=\\([0-9]+\\)/\\([0-9]+\\)" nil t)
                    (list
                     (string-to-number (match-string 1))
                     (string-to-number (match-string 2))))))
      (if lq
          (let ((lq-pct (* 100 (/ (float (nth 0 lq)) (nth 1 lq)))))
            (wifi-format wifi-mode-line-format
                         `((?E . ,essid)
                           (?S . ,(format "%d%%" lq-pct)))))
        "N/A"))))

(define-minor-mode display-wifi-mode
  "Toggle WIFI status display in mode line (Display Wifi mode).
With a prefix argument ARG, enable Display Wifi mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

The text displayed in the mode line is controlled by
`wifi-mode-line-format' and `wifi-status-function'.
The mode line is be updated every `wifi-update-interval'
seconds."
  :global t :group 'wifi
  (setq wifi-mode-line-string "")
  (or global-mode-string (setq global-mode-string '("")))
  (and wifi-update-timer (cancel-timer wifi-update-timer))
  (if (and wifi-status-function wifi-mode-line-format)
      (if (not display-wifi-mode)
	  (setq global-mode-string
		(delq 'wifi-mode-line-string global-mode-string))
	(add-to-list 'global-mode-string 'wifi-mode-line-string t)
	(setq wifi-update-timer (run-at-time nil wifi-update-interval
						'wifi-update-handler))
	(wifi-update))
    (message "Wifi status not available")
    (setq display-wifi-mode nil)))

(defun wifi-update-handler ()
  (wifi-update)
  (sit-for 0))

(defun wifi-update ()
  "Update wifi status information in the mode line."
  (setq wifi-mode-line-string (format-wifi-stats))
  (force-mode-line-update))

(provide 'wifi)
