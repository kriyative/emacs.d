(require 'cl)

(defmacro if-let (binding then &optional else)
  (destructuring-bind (var predicate)
      binding
    `(let ((,var ,predicate))
       (if ,var
           ,then
         ,else))))

(defmacro when-let (binding &rest body)
  `(if-let ,binding
     (progn
       ,@body)))

(defun concat-path (path file)
  (let ((sep (if (boundp 'directory-sep-char) directory-sep-char ?/)))
    (concat path
            (if (eq (aref path (max 0 (1- (length path)))) sep)
                ""
              (char-to-string sep))
            file)))

(defun locate-path (file path-list)
  (when-let (path (find-if '(lambda (path)
                              (let ((file-path (concat-path file path)))
                                (and (file-exists-p file-path) file-path)))
                           path-list))
    (concat-path path file)))

(defun toggle-frame-width ()
  "Toggle between narrow and wide frame layouts"
  (interactive)
  (let ((z-wid (aif (assq 'width initial-frame-alist) (cdr it) 162)))
    (if (< (frame-width) z-wid)
	(set-frame-width (selected-frame) z-wid)
      (set-frame-width (selected-frame) 81))))

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

(defun kill-buffers-matching (pattern)
  "Kill all buffers matching specified regexp"
  (interactive "sRegexp: ")
  (dolist (buffer (remove-if-not
                   (lambda (x) (string-match pattern (buffer-name x)))
                   (buffer-list)))
    (kill-buffer buffer)))

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

(defun tail-f (file)
  "Create a COMINT mode buffer running the `tail -f` command on
specified FILE. If FILE is a ssh/scp style remote file spec,
e.g.,

  user@remote.host.com:/path/to/file.txt

then a ssh connection is opened to remote.host.com, and `tail -f`
is invoked on the remote server."
  (interactive "fFile: ")
  (let ((buf-name (concat "tail-f " file))
	(re "\\(\\w+\\)@\\([^:]+\\):\\(.*\\)"))
    (if (string-match re file)
	(let ((user (match-string 1 file))
	      (host (match-string 2 file))
	      (file1 (match-string 3 file)))
	  (make-comint buf-name "ssh" nil
		       "-l" user
		       host
		       "tail" "-f" file1))
        (make-comint buf-name "tail" nil "-f" (expand-file-name file)))
    (pop-to-buffer (concat "*" buf-name "*"))))

(defun region ()
  (when mark-active
    (buffer-substring (region-beginning) (region-end))))

(defun term-at-point-or-read (&optional label)
  (let* ((word-at-point (word-at-point))
	 (symbol-at-point (symbol-at-point))
	 (default (or word-at-point
		      (and symbol-at-point (symbol-name symbol-at-point)))))
    (read-from-minibuffer (or label "Term: ") default)))

(defun mklist (x) (if (listp x) x (list x)))

(defun toggle-debug-on-error ()
  (interactive)
  (setq debug-on-error (not debug-on-error))
  (message "debug-on-error set to `%s'" debug-on-error))

(defun n-col-view (n)
  (let ((cur (selected-window)))
    (save-excursion 
      (delete-other-windows)
      (let* ((frame-width-cols (/ (frame-pixel-width) (frame-char-width)))
             (pane-width (round (/ (- frame-width-cols n 1) n))))
        (dotimes (i (1- n))
          (split-window-horizontally pane-width)
          (other-window 1)
          (bury-buffer))
        (balance-windows)))
    (select-window cur)))

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

(defun iso-calendar ()
  (interactive)
  (setq european-calendar-style nil)
  (setq calendar-date-display-form
        '(year
          "-"
          (if (< (length month) 2) (concat "0" month) month)
          "-"
          (if (< (length day) 2) (concat "0" day) day)))
  (setq diary-date-forms
        '((year "-" month "-" day "[^0-9]")
          (month "/" day "[^/0-9]")
          (month "/" day "/" year "[^0-9]")
          (monthname " *" day "[^,0-9]")
          (monthname " *" day ", *" year "[^0-9]")
          (dayname "\\W")))
  (cond
   ((string-match "^2[12]" emacs-version)
    (update-calendar-mode-line))
   (t (calendar-update-mode-line))))


(defun partition (seq len)
  (do ((seq seq (nthcdr len seq))
       (result nil (cons (subseq seq 0 len) result)))
      ((null seq) (reverse result))))

(defun join (sep seq)
  (mapconcat 'identity seq sep))

(defun str (x)
  (etypecase x
    (symbol (subseq (symbol-name x) (if (keywordp x) 1 0)))
    (string x)
    (t (prin1-to-string x))))

(defun spaced (seq) (join " " seq))

(defun seq (arg)
  (if (sequencep arg)
      arg
      (list arg)))

(defun html (spec)
  "Generate a string representation of the specified HTML spec."
  (labels ((attr-str (attrs)
             (spaced (mapcar (lambda (x)
                               (destructuring-bind (key val) x
                                 (concat (str key) "=\"" (str val) "\"")))
                             (partition attrs 2)))))
    (if (listp spec)
        (let ((head (first spec)))
          (destructuring-bind (tag &rest attribs) (seq head)
            (join ""
                  (append
                   (list*
                    (concat "<" (str tag)
                            (if (zerop (length attribs))
                                ""
                                (concat " " (attr-str attribs)))
                            ">")
                    (mapcar 'html (rest spec)))
                   (list (concat "</" (str tag) ">"))))))
        spec)))

(defun ext-compile ()
  (interactive)
  (call-interactively (if current-prefix-arg 'remote-compile 'compile)))

