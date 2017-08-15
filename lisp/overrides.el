;;; overrides
;;;

;; view plantuml exported images in an emacs buffer
(defun plantuml-display-image ()
  "Display the rendered image"
  (interactive)
  (let* ((plantuml-file (concat (file-name-sans-extension buffer-file-name) ".png"))
         (plantuml-buf (get-buffer (file-name-nondirectory plantuml-file))))
    (if (not (buffer-live-p plantuml-buf))
	(find-file plantuml-file)
      (progn
	(pop-to-buffer plantuml-buf)
	(revert-buffer nil t nil)))))

;;; fixes issue occasionally seen in rendering mu4e html messages
(defun shr-insert (text)
  (when (and (eq shr-state 'image)
	     (not (bolp))
             text
	     (not (string-match "\\`[ \t\n]+\\'" text)))
    (insert "\n")
    (setq shr-state nil))
  (cond
   ;; ram: adding the following hack to deal with rendering issues
   ;; with some HTML formatted emails in mu4e
   ((listp text)
    (dolist (dom text)
      (shr-descend dom)))
   ((eq shr-folding-mode 'none)
    (insert text))
   ((null text))
   (t
    (when (and (string-match "\\`[ \t\n ]" text)
	       (not (bolp))
	       (not (eq (char-after (1- (point))) ? )))
      (insert " "))
    (dolist (elem (split-string text "[ \f\t\n\r\v ]+" t))
      (when (and (bolp)
		 (> shr-indentation 0))
	(shr-indent))
      ;; No space is needed behind a wide character categorized as
      ;; kinsoku-bol, between characters both categorized as nospace,
      ;; or at the beginning of a line.
      (let (prev)
	(when (and (> (current-column) shr-indentation)
		   (eq (preceding-char) ? )
		   (or (= (line-beginning-position) (1- (point)))
		       (and (shr-char-breakable-p
			     (setq prev (char-after (- (point) 2))))
			    (shr-char-kinsoku-bol-p prev))
		       (and (shr-char-nospace-p prev)
			    (shr-char-nospace-p (aref elem 0)))))
	  (delete-char -1)))
      ;; The shr-start is a special variable that is used to pass
      ;; upwards the first point in the buffer where the text really
      ;; starts.
      (unless shr-start
	(setq shr-start (point)))
      (insert elem)
      (setq shr-state nil)
      (let (found)
	(while (and (> (current-column) shr-width)
		    (> shr-width 0)
		    (progn
		      (setq found (shr-find-fill-point))
		      (not (eolp))))
	  (when (eq (preceding-char) ? )
	    (delete-char -1))
	  (insert "\n")
	  (unless found
	    ;; No space is needed at the beginning of a line.
	    (when (eq (following-char) ? )
	      (delete-char 1)))
	  (when (> shr-indentation 0)
	    (shr-indent))
	  (end-of-line))
	(if (<= (current-column) shr-width)
	    (insert " ")
	  ;; In case we couldn't get a valid break point (because of a
	  ;; word that's longer than `shr-width'), just break anyway.
	  (insert "\n")
	  (when (> shr-indentation 0)
	    (shr-indent)))))
    (unless (string-match "[ \t\r\n ]\\'" text)
      (delete-char -1)))))
