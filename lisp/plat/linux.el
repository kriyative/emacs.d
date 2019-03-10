(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))

(defun dired-x-setup ()
  (define-key dired-mode-map (kbd "C-c o") 'dired-open-file))

(eval-after-load 'dired-x
  '(dired-x-setup))
