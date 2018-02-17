;; a minimal keepass mode using kpcli

(defun keepass (&optional kdb-file)
  (interactive "fFind KDB file: ")
  (let ((buf (make-comint "kpcli" "kpcli" nil
                          "--kdb" (expand-file-name kdb-file))))
    (with-current-buffer buf
      (setq comint-password-prompt-regexp
            (concat "\\(^Please provide the master password:\\|"
                    comint-password-prompt-regexp
                    "\\)")))
    (switch-to-buffer buf)))
