(require 'org)
(require 'org-passwords)
(require 'password-mode)

(defvar pwcrypt-file (expand-file-name "~/pwcrypt.org.gpg"))

(defmacro with-pwcrypt (file &rest body)
  (declare (indent defun))
  `(with-current-buffer (find-file-noselect ,file)
     (goto-char (point-min))
     ,@body))

(defun pwcrypt-find-all (name)
  (with-pwcrypt pwcrypt-file
    (let ((match-points '()))
      (while (re-search-forward (concat "^**[ ]*" "\\(" name ".*$\\)") nil t)
        (let* ((beg (match-beginning 1))
               (match-str (buffer-substring-no-properties beg (match-end 1))))
          (setq match-points (cons (cons match-str beg) match-points))))
      (reverse match-points))))

;; (pwcrypt-find-all "Bitbucket")
;; (pwcrypt-find-all "Costco")

(defun pwcrypt-find-first (name)
  (first (pwcrypt-find-all name)))

(defun pwcrypt-get (loc)
  (with-pwcrypt pwcrypt-file
    (let ((get1 (lambda (loc)
                  (reduce (lambda (m x)
                            (destructuring-bind (k pk)
                                x
                              (append m (list k (org-entry-get (cdr loc) pk t)))))
                          `((:username ,org-passwords-username-property)
                            (:password ,org-passwords-password-property)
                            (:url ,org-passwords-url-property)
                            (:notes "NOTES"))
                          :initial-value '()))))
      (if (consp (cdr loc))
          (mapcar (lambda (x) (funcall get1 x)) loc)
        (funcall get1 loc)))))

;; (pwcrypt-get (first (pwcrypt-find-all "Hipchat")))
;; (getf (pwcrypt-get (first (pwcrypt-find-all "Github"))) :username)
;; (pwcrypt-get (pwcrypt-find-all "Github"))

(defun pwcrypt-index-extract-item ()
  (let ((pt (point)))
    (cons (buffer-substring-no-properties (+ 3 pt) (point-at-eol)) pt)))

(defun pwcrypt-index ()
  (with-pwcrypt pwcrypt-file
    (when (search-forward "* Logins")
      (cdr (org-map-entries 'pwcrypt-index-extract-item t 'tree)))))

;; (pwcrypt-index)
;; (pwcrypt-get (third (pwcrypt-index)))

(defun pwcrypt-read-entry ()
  (let ((completion-ignore-case t))
    (list
     (completing-read "Entry: "
                      (mapcar 'car (pwcrypt-index))))))

(defun pwcrypt-get-property (key property)
  (getf (pwcrypt-get (pwcrypt-find-first key)) property))

(defun pwcrypt-copy-property (key property)
  (let ((v (pwcrypt-get-property key property)))
    (kill-new v)
    (funcall interprogram-cut-function v)))

(defun pwcrypt-copy-username (arg)
  (interactive (pwcrypt-read-entry))
  (pwcrypt-copy-property arg :username))

(defun pwcrypt-copy-password (arg)
  (interactive (pwcrypt-read-entry))
  (pwcrypt-copy-property arg :password))

(defun pwcrypt-visit-url (arg)
  (interactive (pwcrypt-read-entry))
  (let ((url (pwcrypt-get-property arg :url)))
    (if url
        (browse-url-default-browser url)
      (message "No url for %s" arg))))

(defun pwcrypt-show-notes (arg)
  (interactive (pwcrypt-read-entry))
  (let ((notes (pwcrypt-get-property arg :notes))
        (buf (get-buffer-create "*pwcrypt-notes*")))
    (with-current-buffer buf
      (password-mode -1)
      (setq buffer-read-only nil)
      (delete-region (point-min) (point-max))
      (insert "Notes for " arg "\n\n")
      (when notes
        (save-excursion
          (insert notes)
          (insert "\n"))
        (while (search-forward "\\n" nil t)
          (replace-match "\n" nil t)))
      (help-mode)
      (password-mode)
      (setq buffer-read-only t)
      (goto-char (point-max)))
    (pop-to-buffer buf)))

(defun pwcrypt-encode-notes (p1 p2)
  (interactive "r")
  (goto-char p1)
  (while (search-forward "\n" p2 t)
    (replace-match "\\n" nil t)))

(provide 'pwcrypt)
