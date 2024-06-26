(defun run-refresh-exec (command)
  (setq buffer-read-only nil)
  (unwind-protect
      (progn
        (delete-region (point-min) (point-max))
        (insert (format "# %s\n\n" command))
        (goto-char (point-max))
        (let ((shell-command-dont-erase-buffer t))
          (shell-command command
                         (current-buffer)
                         (current-buffer)))
        (goto-char (point-max))
        (insert (format "\n# updated: %s"
                        (format-time-string "%a %H:%M:%S"))))
    (setq buffer-read-only t)))

(defvar-local run-refresh--command nil)

(defun run-refresh-redo ()
  (interactive)
  (run-refresh-exec run-refresh--command))

(defun run-refresh-modify (&optional command)
  (interactive "sCommand: ")
  (setq run-refresh--command command)
  (run-refresh-redo))

(defun run-refresh-quit ()
  (interactive)
  (kill-buffer))

(defun run-refresh (&optional command)
  (interactive "sCommand: ")
  (let ((buf (generate-new-buffer (format "*run: %s*" command))))
    (with-current-buffer buf
      ;; (make-local-variable 'run-refresh--command)
      (setq buffer-read-only t)
      (local-set-key "g" 'run-refresh-redo)
      (local-set-key "u" 'run-refresh-modify)
      (local-set-key "q" 'run-refresh-quit)
      (setq run-refresh--command command)
      (run-refresh-redo))
    (pop-to-buffer buf)))

(defun run-watch-kill-buffer-hook ()
  (when (buffer-local-boundp 'watch-timer (current-buffer))
    (message "Canceling timer for %S" (current-buffer))
    (cancel-timer (buffer-local-value 'watch-timer (current-buffer)))))

(defun run-watch (&optional command interval)
  (interactive "sCommand: \nnInterval (in seconds): ")
  (let ((buf (generate-new-buffer "*watch*")))
    (let ((dofn (lambda (buf command)
                  (with-current-buffer buf
                    (delete-region (point-min) (point-max))
                    (shell-command (concat command " &")
                                   (current-buffer)
                                   (current-buffer))
                    (goto-char (point-max))
                    (insert (format "\n;; Updated %s"
                                    (format-time-string "%a %H:%M:%S")))))))
      (with-current-buffer buf
        (add-hook 'kill-buffer-hook 'run-watch-kill-buffer-hook)
        (setq-local watch-timer
                    (run-with-timer 0 interval dofn buf command))))
    (pop-to-buffer buf)))

(provide 'watch-proc)
