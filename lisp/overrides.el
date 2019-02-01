;;; overrides
;;;

;; view plantuml exported images in an emacs buffer
(defun plantuml-display-image ()
  "Display the rendered image"
  (interactive)
  (let* ((plantuml-file (concat (file-name-sans-extension buffer-file-name) ".png"))
         (plantuml-buf (get-buffer (file-name-nondirectory plantuml-file))))
    (clear-image-cache plantuml-file)
    (if (not (buffer-live-p plantuml-buf))
	(find-file plantuml-file)
      (progn
	(pop-to-buffer plantuml-buf)
        (with-current-buffer plantuml-buf
          (revert-buffer nil t nil))))))

;;; slight fix to prevent accidentally clobbering compose settings
(defun mu4e-multi-compose-set-account (&optional account)
  "Set the ACCOUNT for composing.
With Optional Argument ACCOUNT, set all variables for that given
identifier, else it tries to retrieve the message in context and
detect ACCOUNT from it."
  (interactive)
  (let* ((msg (or mu4e-compose-parent-message
                  (ignore-errors (mu4e-message-at-point))))
         (account (or account
                      (mu4e-multi-get-msg-account msg)))
         (account-vars (cdr (assoc account mu4e-multi-account-alist))))
    (when account-vars
      (mapc #'(lambda (var)
                (set (make-local-variable (car var)) (cdr var)))
            account-vars)
      (when (memq major-mode '(mu4e-compose-mode message-mode))
        (message-remove-header "from")
        (message-add-header (format "From: %s\n" (message-make-from)))
        (message "Using account %s" account)))))

;;; modify mark-for-trash in mu4e to not set the +T flag which
;;; confuses Gmail into retaining messages in the Trash folder forever
(setq mu4e-marks
      (cons `(trash
              :char ("d" . "▼")
              :prompt "dtrash"
              :dyn-target ,(lambda (target msg) (mu4e-get-trash-folder msg))
              :action ,(lambda (docid msg target)
                         (mu4e~proc-move docid
                                         (mu4e~mark-check-target target) "-N")))
            (remove-if (lambda (x)
                         (equal 'trash (car x)))
                       mu4e-marks)))

(defun mu4e-maildirs-extension-index-updated-handler ()
  "Handler for `mu4e-index-updated-hook'."
  (mu4e-maildirs-extension-force-update '(16)))

(defun geiser-set-scheme* (impl)
  (geiser-impl--set-buffer-implementation impl)
  (geiser-repl--set-up-repl impl)
  (geiser-syntax--add-kws)
  (geiser-syntax--fontify))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; browse-url

(defun browse-url-can-use-xdg-open ()
  "Return non-nil if the \"xdg-open\" program can be used.
xdg-open is a desktop utility that calls your preferred web browser.
This requires you to be running either Gnome, KDE, Xfce4 or LXDE."
  (and (getenv "DISPLAY")
       (executable-find "xdg-open")
       ;; xdg-open may call gnome-open and that does not wait for its child
       ;; to finish.  This child may then be killed when the parent dies.
       ;; Use nohup to work around.  See bug#7166, bug#8917, bug#9779 and
       ;; http://lists.gnu.org/archive/html/emacs-devel/2009-07/msg00279.html
       (executable-find "nohup")
       (or (getenv "GNOME_DESKTOP_SESSION_ID")
	   ;; GNOME_DESKTOP_SESSION_ID is deprecated, check on Dbus also.
	   (condition-case nil
	       (eq 0 (call-process
		      "dbus-send" nil nil nil
                      "--dest=org.gnome.SessionManager"
                      "--print-reply"
                      "/org/gnome/SessionManager"
                      "org.gnome.SessionManager.CanShutdown"))
	     (error nil))
	   (equal (getenv "KDE_FULL_SESSION") "true")
	   (condition-case nil
	       (eq 0 (call-process
		      "/bin/sh" nil nil nil
		      "-c"
		      ;; FIXME use string-match rather than grep.
		      "xprop -root _DT_SAVE_MODE|grep xfce4"))
	     (error nil))
	   (member (getenv "DESKTOP_SESSION") '("LXDE" "Lubuntu" "stumpwm"))
	   (equal (getenv "XDG_CURRENT_DESKTOP") "LXDE"))))

;; from:
;; https://emacs.stackexchange.com/questions/17283/is-it-possible-to-get-prettified-symbols-in-org-mode-source-blocks
(defun org-src-font-lock-fontify-block (lang start end)
  "Fontify code block.
     This function is called by emacs automatic fontification, as long
     as `org-src-fontify-natively' is non-nil."
  (let ((lang-mode (org-src--get-lang-mode lang)))
    (when (fboundp lang-mode)
      (let ((string (buffer-substring-no-properties start end))
            (modified (buffer-modified-p))
            (org-buffer (current-buffer)) pos next)
        (remove-text-properties start end '(face nil))
        (with-current-buffer
            (get-buffer-create
             (concat " org-src-fontification:" (symbol-name lang-mode)))
          (delete-region (point-min) (point-max))
          (insert string " ") ;; so there's a final property change
          (unless (eq major-mode lang-mode) (funcall lang-mode))
          ;; Avoid `font-lock-ensure', which does not display fonts in
          ;; source block.
          (font-lock-fontify-buffer)
          (setq pos (point-min))
          (while (setq next (next-single-property-change pos 'face))
            (put-text-property
             (+ start (1- pos)) (1- (+ start next)) 'face
             (get-text-property pos 'face) org-buffer)
            (setq pos next))
          ;; Addition: also copy 'composition info for prettified symbols
          (setq pos (point-min))
          (while (setq next (next-single-property-change pos 'composition))
            (put-text-property
             (+ start (1- pos)) (1- (+ start next)) 'composition
             (get-text-property pos 'composition) org-buffer)
            (setq pos next))
          ;; End addition
          )
        (add-text-properties
         start end
         '(font-lock-fontified t fontified t font-lock-multiline t))
        (set-buffer-modified-p modified)))))
