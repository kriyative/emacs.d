(defun toggle-option (args option)
  (if (find option args)
      (remove option args)
    (concat args (string option))))

(defvar ps-args "auxh --sort -pcpu")
;; (set-default 'ps-args "auxh --sort -pcpu")

(defun ps-exec (&optional args)
  (save-excursion
    (when args (setq ps-args args))
    (shell-command (concat "ps " ps-args) (current-buffer))
    (goto-char (point-min))
    (let* ((header (buffer-substring (point-min) (line-end-position)))
           (header (replace-regexp-in-string "%" "%%" header)))
      (delete-region (point-min) (+ 1 (line-end-position)))
      (setq header-line-format (concat " " header)))
    (goto-char (point-max))
    (insert "\n;; ps " ps-args "\n")
    (setq buffer-read-only t)))

(defmacro def-key-actions (&optional key action &rest clauses)
  (when (and key action)
    `(progn
       (local-set-key ,key
                      (if (or (functionp ,action)
                              (fboundp ,action))
                          ,action
                        (lambda ()
                          (interactive)
                          ,action)))
       (def-key-actions ,@clauses))))

(defun ps-kill-process (&optional arg)
  (interactive)
  (save-excursion
    (let ((bound (progn
                   (end-of-line)
                   (point))))
      (beginning-of-line)
      (if (re-search-forward "[ ]*\\([0-9]+\\) " bound t)
          (let ((pid (string-to-int (match-string 1))))
            (when (and (> pid 0)
                       (y-or-n-p (format "Kill pid %d - " pid)))
              (let ((sig (if arg
                             (let ((completion-ignore-case t))
                               (completing-read "Signal: "
                                                '("HUP"
                                                  "INT"
                                                  "QUIT"
                                                  "ABRT"
                                                  "KILL"
                                                  "ALRM"
                                                  "TERM")))
                           "INT")))
                (shell-command (format "kill -%s %d" sig pid)))
              (sleep-for 0 500)
              (ps-exec)))
        (message "No pid on this line")))))

(defun make-ps-buffer (buf-name)
  (let ((buf (get-buffer-create buf-name)))
    (save-excursion
      (set-buffer buf)
      (make-local-variable 'ps-args)
      (make-local-variable 'buffer-read-only)
      (def-key-actions
        "g" (ps-exec)
        "a" (ps-exec (toggle-option ps-args ?a))
        "u" (ps-exec (toggle-option ps-args ?u))
        "m" (ps-exec (toggle-option ps-args ?m))
        "c" (ps-exec (toggle-option ps-args ?c))
        "x" (ps-exec (toggle-option ps-args ?x))
        "w" (ps-exec (toggle-option ps-args ?w))
        "A" (ps-exec "auxw")
        "q" (bury-buffer)
        "k" 'ps-kill-process)
      (set-all-line-truncation t)
      (ps-exec))
    buf))  

(defun ps ()
  (interactive)
  (let ((buf-name "*ps*"))
    (pop-to-buffer
     (or (get-buffer buf-name) (make-ps-buffer buf-name)))))
