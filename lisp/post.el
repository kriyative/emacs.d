;; things to run at the end of emacs initialization

(when org-agenda-files
  (add-hook 'display-time-hook 'show-daily-agenda))

;;; overrides
;;;
(defun plantuml-display-image()
  "Display the rendered image"
  (interactive)
  (let* ((plantuml-file (concat (file-name-sans-extension buffer-file-name) ".png"))
         (plantuml-buf (get-buffer (file-name-nondirectory plantuml-file))))
    (if (not (buffer-live-p plantuml-buf))
	(find-file plantuml-file)
      (progn
	(pop-to-buffer plantuml-buf)
	(revert-buffer nil t nil)))))
