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
  (concat (file-name-as-directory path) file))

(defun locate-path (file path-list)
  (when-let (path (find-if (lambda (path)
                             (let ((file-path (concat-path path file)))
                               (and (file-exists-p file-path) file-path)))
                           path-list))
    (concat-path path file)))

(defvar base-exec-path exec-path)

(defun add-exec-paths (exec-paths &optional append)
  (dolist (path exec-paths)
    (let ((path (expand-file-name path)))
      (when (file-directory-p path)
        (add-to-list 'exec-path path append)))
    (setenv "PATH" (join ":" exec-path))))

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
   (t
    (when (fboundp 'calendar-update-mode-line)
      (calendar-update-mode-line)))))

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

(defun re-matches (re str)
  (when (string-match re str)
    (mapcar (lambda (match)
              (apply 'substring str match))
            (partition (match-data) 2))))

(defmacro bind (clauses &rest body)
  "This macro combines the behaviour of the forms `let*',
`destructuring-bind', and `multiple-value-bind', permitting the
following style of binding form:

  (bind (((:values m n) (values 10 20))
         ((a b _c &key (d 10)) '(1 2 3))
         (x 5))
    (+ x a b d m n))
  => 48

Note in the destructuring form (a b _c &key (d 10)), _c is a short form
for declaring it as ignorable.

This is a more limited and lightweight implementation of some ideas from
metabang-bind (http://common-lisp.net/project/metabang-bind/)."
  (labels
      ((parse-arglist (args)
         (loop
            for arg in args
            collect arg into args
            when (and (symbolp arg) (eq (aref (symbol-name arg) 0) ?_))
            collect arg into ignorables
            finally (return (values args ignorables))))
       (cons-form (form args clauses body)
         (multiple-value-bind (arglist ignorables)
             (parse-arglist args)
           `(,form ,arglist
                   ,@(cdar clauses)
                   ,@(when ignorables `((declare ,(list* 'ignore ignorables))))
                   (bind ,(cdr clauses) ,@body)))))
    (cond
      ((null clauses) `(progn ,@body))
      ((sequencep (caar clauses))
       (cond
         ((eq (caaar clauses) :values)
          (cons-form 'multiple-value-bind (cdaar clauses) clauses body))
         ((eq (caaar clauses) :slots)
          `(with-slots ,(cdaar clauses) ,@(cdar clauses)
             (bind ,(cdr clauses) ,@body)))
         (t
          (cons-form 'destructuring-bind (caar clauses) clauses body))))
      (t
       `(let (,(car clauses))
          (bind ,(cdr clauses) ,@body))))))

(defun emacs-version-info ()
  (bind (((_ &rest matches) (re-matches
                             "\\([0-2][0-9]*\\)\.\\([0-9]*\\).\\([0-9]*\\)"
                             emacs-version)))
    (mapcar 'string-to-number matches)))

(defun emacs24-or-newer-p () (<= 24 (first (emacs-version-info))))
(defun emacs24p () (= 24 (first (emacs-version-info))))
(defun emacs23p () (= 23 (first (emacs-version-info))))
(defun emacs22p () (= 22 (first (emacs-version-info))))

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

(defun add-exec-paths (paths)
  (let ((paths (remove-if-not 'file-directory-p paths)))
    (dolist (path paths)
      (add-to-list 'exec-path path))
    (setenv "PATH" (join ":" exec-path))))

(defun set-all-line-truncation (v)
  (make-local-variable 'truncate-lines)
  (setq truncate-lines v)
  (make-local-variable 'truncate-partial-width-windows)
  (setq truncate-partial-width-windows v))

(defun turn-on-line-truncation ()
  (interactive)
  (set-all-line-truncation t))

(defmacro with-cwd (dir &rest body)
  (let ((cwd% (gensym)))
    `(let ((,cwd% default-directory))
       (unwind-protect
           (progn
             (cd-absolute ,dir)
             ,@body)
         (cd-absolute ,cwd%)))))

(defmacro save-values (vars &rest body)
  (let ((tmpargs (mapcar (lambda (x) (gensym x)) vars)))
    `(let ,(map 'list
                (lambda (tmparg var)
                  (list tmparg var))
                tmpargs
                vars)
       (unwind-protect
           (progn ,@body)
         ,@(map 'list
                (lambda (var tmparg)
                  (list 'setq var tmparg))
                vars
                tmpargs)))))

(defun test-string-match (item x) (string-match x item))

(defun find-file-no-desktop (path)
  (when (file-exists-p f)
    (let ((buf (find-file-noselect f)))
      (when (featurep 'desktop)
        (push (buffer-name buf)
              desktop-clear-preserve-buffers)))))
