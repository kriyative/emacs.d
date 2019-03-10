(require 'cl)

(defmacro if-bind (binding then &optional else)
  (declare (indent 1))
  (destructuring-bind (var predicate)
      binding
    `(let ((,var ,predicate))
       (if ,var
           ,then
         ,else))))

(defmacro when-bind (binding &rest body)
  (declare (indent 1))
  `(if-bind ,binding
     (progn
       ,@body)))

(defun concat-path (path file)
  (concat (file-name-as-directory path) file))

(defun locate-path (file path-list)
  (when-bind (path (find-if (lambda (path)
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

(defun region ()
  (when mark-active
    (buffer-substring (region-beginning) (region-end))))

(defun term-at-point-or-read (&optional label)
  (let* ((word-at-point (word-at-point))
	 (symbol-at-point (symbol-at-point))
	 (default (or word-at-point
		      (and symbol-at-point (symbol-name symbol-at-point)))))
    (read-from-minibuffer (or label "Term: ") default)))

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


(defun spaced (seq) (join " " seq))

(defun re-matches (re str)
  (when (string-match re str)
    (mapcar (lambda (match)
              (apply 'substring str match))
            (partition (match-data) 2))))

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
                             (partition attrs 2))))
           (seq (x)
             (if (listp x) x (list x))))
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

(defun test-string-match (item x) (string-match x item))

(defun find-file-if-exists (path &optional find-file-function find-file-args)
  (let ((path (expand-file-name path)))
    (when (file-exists-p path)
      (apply (or find-file-function 'find-file-noselect)
             path
             find-file-args))))

(defun find-file-no-desktop (path)
  (require 'desktop)
  (when-bind (buf (find-file-if-exists f))
    (when (featurep 'desktop)
      (push (buffer-name buf)
	    desktop-clear-preserve-buffers))))

(defun load-file-if-exists (path &rest load-args)
  (let ((path (and (stringp path)
		   (expand-file-name path))))
    (when (file-exists-p path)
      (apply 'load path load-args))))

(defun try-require (feature)
  (condition-case nil
      (require feature)
    (error (message "Error loading feature %s" feature))))

(defun compilation-mode-colorize-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))

(defun setup-ansi-color ()
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'compilation-filter-hook 'compilation-mode-colorize-buffer)
  (add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply))

(defun parse-relative-time (time-str)
  (destructuring-bind (sec min hour day month year dow dst zone)
      (parse-time-string time-str)
    (destructuring-bind (sec1 min1 hour1 day1 month1 year1 dow1 dst1 zone1)
	(decode-time)
      (encode-time (or sec sec1)
		   (or min min1)
		   (or hour hour1)
		   (or day day1)
		   (or month month1)
		   (or year year1)
		   (or zone zone1)))))

;; (time-less-p (parse-relative-time "9 am") (current-time))

;;; https://www.emacswiki.org/emacs/AddCommasToNumbers
(defun add-number-grouping (number &optional separator)
  "Add commas to NUMBER and return it as a string. Optional
SEPARATOR is the string to use to separate groups. It defaults to
a comma."
  (when number
    (let ((num (if (stringp number)
                   number
                 (number-to-string number)))
          (op (or separator ",")))
      (while (string-match "\\(.*[0-9]\\)\\([0-9][0-9][0-9].*\\)" num)
        (setq num (concat
                   (match-string 1 num) op
                   (match-string 2 num))))
      num)))

;; adapted from:
;; https://stackoverflow.com/questions/900372/in-emacs-how-do-i-change-the-minibuffer-completion-list-window
(defun vertical-display-completions (buf alist)
  "put the *completions* buffer in the rightmost vertical window"
  (let ((windows (delete (minibuffer-window) (window-list))))
    (when (eq 1 (length windows))
      (with-selected-window (car windows)
        (split-window-horizontally)))
    (let ((target-window (window-at (frame-width) (- (frame-height) 2)))
          (pop-up-windows t))
      (set-window-buffer target-window buf)
      target-window)))

(defun assoc-cdr (key alist)
  (cdr (assoc key alist)))

(defun exec! (command)
  (interactive (list (read-shell-command "$ ")))
  (let* ((program-and-args (if (stringp command)
                               (split-string command)
                             command))
         (program (car program-and-args))
         (args (cdr program-and-args))
         (console (get-buffer-create "*Console*"))
         (pt (with-current-buffer console
               (goto-char (point-max))
               (insert "$ "
                       (mapconcat 'identity program-and-args " ")
                       "\n")
               (point)))
         (ret (apply 'call-process program nil console t args))
         (out (with-current-buffer console
                (buffer-substring-no-properties pt (point)))))
    (if (and (numberp ret) (= 0 ret))
        out
      (throw 'exec!-error (list ret out)))))

(defun spawn& (command)
  (interactive (list (read-shell-command "$ ")))
  (let* ((program-and-args (if (stringp command)
                               (split-string command)
                             command))
         (program (car program-and-args))
         (program-name (file-name-nondirectory program))
         (program-buffer (concat " *" program-name))
         (args (cdr program-and-args)))
    (apply 'start-process program-name program-buffer program args)))

(defvar *default-start-level* 3)

(defun startup-emacs (&optional level)
  (dotimes (i (1+ (or level *default-start-level*)))
    (dolist (f (directory-files "~/.emacs.d/startup"
                                t
                                (format "%02d.*" i)))
      (load f))))

(require 'url)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

(require 'el-get)

(defmacro my-el-get-bundles (&rest rcps)
  `(progn
     ,@(mapcar
        (lambda (rcp)
          `(el-get-bundle ,@(if (listp rcp) rcp (list rcp))))
        rcps)))
