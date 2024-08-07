(defun mu4e-get-inbox-maildirs ()
  (cl-remove-if-not (lambda (x)
                      (string-match "INBOX$" x))
                    (mu4e-get-maildirs)))

(defun rk-mu4e-ask-inbox (prompt)
  "Ask the user for a shortcut (using PROMPT) as defined in
`mu4e-maildir-shortcuts', then return the corresponding folder
name. If the special shortcut 'o' (for _o_ther) is used, or if
`mu4e-maildir-shortcuts' is not defined, let user choose from all
maildirs under `mu4e-maildir'."
  (let ((prompt (mu4e-format "%s" prompt)))
    (format
     "/%s/INBOX"
     (funcall mu4e-completing-read-function
              prompt
              (mapcar (lambda (maildir)
                        (cons (progn
                                (string-match "\/\\(.*\\)\/" maildir)
                                (match-string 1 maildir))
                              maildir))
                      (mu4e-get-inbox-maildirs))
              nil
              t))))

(defun mu4e~headers-jump-to-inbox (maildir)
  "Show the messages in maildir (user is prompted to ask what
maildir)."
  (interactive
   (let ((maildir (rk-mu4e-ask-inbox "Jump to INBOX: ")))
     (list maildir)))
  (when maildir
    (mu4e-mark-handle-when-leaving)
    (mu4e-headers-search
     (format "maildir:\"%s\"" maildir))))

(defun mu4e-headers-mode-hook ()
  (setq mu4e-split-view (if (< 128 (frame-width)) 'vertical 'horizontal)
        mu4e-headers-visible-columns (/ (frame-width) 3)))

(defvar rk--mu4e-view-message nil)

(defun rk--mu4e-view-func (msg)
  (setq rk--mu4e-view-message msg)
  (mu4e~headers-view-handler msg))

(defun mu4e-view-mode-hook ()
  (if (and rk--mu4e-view-message
           (mu4e-message-field rk--mu4e-view-message :body-html)
           (or (not (mu4e-message-field rk--mu4e-view-message :body-txt))
               (mu4e~message-use-html-p rk--mu4e-view-message mu4e-view-prefer-html)))
      (progn
        (setq truncate-lines t)
        (visual-line-mode -1))
    (visual-line-mode 1))
  (set (make-local-variable 'browse-url-browser-function)
       'browse-url-default-browser)
  (setq shr-width nil))

;;; overrides shr-tag-base to workaround cases where dom-attribute
;;; base has an empty string href (why?)
(defun shr-tag-base (dom)
  (let ((base (dom-attr dom 'href)))
    (when (and base (< 0 (length base)))
      (setq shr-base (shr-parse-base base))))
  (shr-generic dom))

(defun mu4e-compose-mode-hook ()
  (setq mu4e-compose-format-flowed t
        mu4e-compose-dont-reply-to-self t
        mu4e-compose-keep-self-cc nil
        mu4e-compose-complete-addresses t
        use-hard-newlines nil))

(defun mu4e-action-view-in-system-browser (msg)
  (let ((browse-url-browser-function 'browse-url-default-browser))
    (mu4e-action-view-in-browser msg)))

(defun mu4e-message-mode-hook ()
  (local-set-key "\C-c\M-o" 'org-mime-htmlize)
  (auto-fill-mode -1)
  (visual-line-mode 1))

(defun rk--mu4e-sent-messages-behavior ()
  (let ((maildir (getf mu4e-compose-parent-message :maildir)))
    (if (or (and maildir (string-match "/gmail/" maildir))
            (equal "gmail" mu4e-multi-last-read-account))
        'delete
      'sent)))

(use-package mu4e
  :straight (mu4e :files ("build/mu4e/*.el"
                          "build/mu4e/*.elc"
                          "build/mu4e/*.info")
                  :pre-build (("./autogen.sh")
                              ("ninja" "-C" "build")))
  :custom (mu4e-mu-binary (expand-file-name "build/mu/mu"
                                            (straight--repos-dir "mu")))
  :config
  (setq mu4e-maildir "~/Mail" ;; top-level Maildir
        ;; mu4e-get-mail-command "mbsync-all -u"
        mu4e-get-mail-command "/bin/true"
        mu4e-update-interval nil
        mu4e-search-results-limit 1000
        ;; mu4e-update-interval nil
        ;; fix for duplicate UID per:
        ;; http://pragmaticemacs.com/emacs/fixing-duplicate-uid-errors-when-using-mbsync-and-mu4e/
        mu4e-change-filenames-when-moving t
        mu4e-headers-skip-duplicates t
        mu4e-view-prefer-html nil
        mu4e-hide-index-messages t
        mu4e-split-view 'vertical
        mu4e-headers-fields '((:human-date .  12) ;; alternatively, use :human-date
                              ;; (:flags      .   6)
                              (:from       .  24)
                              (:subject    .  nil))
        mu4e-sent-messages-behavior 'rk--mu4e-sent-messages-behavior
        mu4e-view-show-addresses t
        ;; mu4e-view-mode-hook '(bbdb-mua-auto-update)
        org-mu4e-convert-to-html nil
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "msmtp"
        message-kill-buffer-on-exit t
        mu4e-headers-leave-behavior 'apply
        mu4e-html2text-command 'mu4e-shr2text
        ;; mu4e-html2text-command "html2text -utf8 -width 72"
        mu4e-doc-dir mu4e-builddir
        mu4e-use-fancy-chars nil
        mu4e-headers-thread-child-prefix '("" . "")
        mu4e-headers-thread-last-child-prefix '("  " . "  ")
        mu4e-headers-thread-connection-prefix '("" . "")
        mu4e-headers-thread-blank-prefix '("" . "")
        mu4e-index-cleanup t
        ;; mu4e-index-cleanup nil      ;; don't do a full cleanup check
        mu4e-index-lazy-check nil
        ;; mu4e-index-lazy-check t     ;; don't consider up-to-date dirs
        org-export-with-toc nil
        mu4e-view-use-gnus nil ;; don't use gnus to render
        gnus-inhibit-images t
        mu4e-completing-read-function 'completing-read
        mu4e-view-func 'rk--mu4e-view-func)
  (add-to-list 'mu4e-view-actions
               '("view in browser" . mu4e-action-view-in-system-browser))
  (add-hook 'mu4e-headers-mode-hook 'mu4e-headers-mode-hook)
  (add-hook 'mu4e-view-mode-hook 'mu4e-view-mode-hook)
  ;; (add-hook 'mu4e-compose-mode-hook 'org-mu4e-compose-org-mode)
  (add-hook 'message-mode-hook 'mu4e-message-mode-hook)
  (add-hook 'mu4e-compose-mode-hook 'message-mode-hook)
  (add-hook 'mu4e-compose-mode-hook 'mu4e-compose-mode-hook)
  (add-to-list 'mu4e-bookmarks
               '("flag:flagged AND NOT flag:trashed"
                 "Flagged messages" 102))
  (custom-set-faces
   '(mu4e-unread-face ((t (:foreground "blue"))))
   '(mu4e-header-highlight-face ((t (:inherit hl-line :extend t)))))

  :bind
  (:map mu4e-main-mode-map
        ("i" . mu4e~headers-jump-to-inbox))
  (:map mu4e-view-mode-map
        ("C-c u" . mu4e-view-save-url)))

(defun mu4e~headers-human-date (msg)
  "Show a 'human' date.
If the date is today, show the time, otherwise, show the
date. The formats used for date and time are
`mu4e-headers-date-format' and `mu4e-headers-time-format'."
  (let ((date (mu4e-msg-field msg :date)))
    (if (equal date '(0 0 0))
        "None"
      (let ((day1 (decode-time date))
            (day2 (decode-time (current-time))))
        (cond
	 ((not (eq (nth 5 day1) (nth 5 day2))) ; year
	  (format-time-string "%d-%b-%Y" date))
	 ((and (eq (nth 4 day1) (nth 4 day2))
               (eq (nth 3 day2) (nth 3 day1)))
	  (format-time-string "%R" date))
	 (t (format-time-string "%d-%b" date)))))))

;; Mitigate Bug#28350 (security) in Emacs 25.2 and earlier.
(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))

(use-package mu4e-multi
  :straight (mu4e-multi :type git
                        :host github
                        :repo "kriyative/mu4e-multi")
  :demand t
  :bind
  (("C-x m" . mu4e-multi-compose-new))
  :config
  (mu4e-multi-enable)
  (remove-hook 'message-mode-hook 'mu4e-multi-compose-set-account)
  (add-hook 'mu4e-compose-mode-hook 'mu4e-multi-compose-set-account)
  (add-hook 'message-send-mail-hook 'mu4e-multi-smtpmail-set-msmtp-account))

(defun message-mode-hook ()
  (setq message-fill-column nil
        message-from-style 'angles
        message-citation-line-function 'message-insert-citation-line
        message-cite-style nil
        message-yank-prefix "> "
        message-yank-cited-prefix "> "
        message-yank-empty-prefix "> "
        ;; message-yank-prefix "  "
        ;; message-yank-cited-prefix "  "
        ;; message-yank-empty-prefix "  "
        ))

(use-package message
  :config
  (add-hook 'message-mode-hook 'message-mode-hook))

(defun rk--mbsync-sync-update (info)
  (when (cdr info)
    (message "mu4e update %S" info)
    (mu4e-update-mail-and-index t)))

(use-package mbsync
  :straight (mbsync :type git
                    :host github
                    :repo "kriyative/mbsync.el")
  :config
  (add-hook 'mbsync-after-sync-hook 'rk--mbsync-sync-update))

(defun rk-mbsync-start ()
  (dolist (spec *mbsync-accounts*)
    (cl-destructuring-bind (account &optional interval)
        (if (listp spec) spec (list spec))
      (unless (assoc account *mbsync--timers*)
        (mbsync--sync-start account interval)))))

(defun rk-mbsync-stop ()
  (interactive)
  (dolist (spec *mbsync-accounts*)
    (let* ((account (if (listp spec) (car spec) spec))
           (existing (assoc account *mbsync--timers*)))
      (when existing
        (cancel-timer (cdr existing)))))
  (setq *mbsync--timers* nil))

(defun rk-mail ()
  (interactive)
  (rk-mbsync-start)
  (mu4e))

;;;;;;;;;;;;;;;; overrides ;;;;;;;;;;;;;;;;

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
                         (mu4e--server-move docid
                                            (mu4e--mark-check-target target)
                                            "-N")))
            (cl-remove-if (lambda (x)
                            (equal 'trash (car x)))
                          mu4e-marks)))

(defun mu4e~view-make-urls-clickable ()
  "Turn things that look like URLs into clickable things.
Also number them so they can be opened using `mu4e-view-go-to-url'."
  (let ((num 0))
    (save-excursion
      (setq mu4e~view-link-map ;; buffer local
            (make-hash-table :size 32 :weakness nil))
      (goto-char (point-min))
      (while (re-search-forward mu4e~view-beginning-of-url-regexp nil t)
        (let ((bounds (thing-at-point-bounds-of-url-at-point)))
          (when bounds
            (let* ((url (thing-at-point-url-at-point))
                   (ov (make-overlay (car bounds) (cdr bounds))))
              (puthash (cl-incf num) url mu4e~view-link-map)
              (add-text-properties
               (car bounds)
               (cdr bounds)
               `(face mu4e-link-face
                      mouse-face highlight
                      mu4e-url ,url
                      keymap ,mu4e-view-clickable-urls-keymap
                      help-echo
                      "[mouse-1] or [M-RET] to open the link"))
              (overlay-put ov 'invisible t)
              (overlay-put ov 'help-echo url)
              (overlay-put ov 'after-string
                           (propertize (format "\u200B[%d]" num)
                                       'face 'mu4e-url-number-face)))))))))

(defun rk-maildir-fix-unread-trash (&optional root-dir)
  "Mark any unread Maildir files in Trash folders as SEEN."
  (interactive "DMaildir Root: ")
  (dolist (f (directory-files-recursively root-dir ",[^S]*$"))
    (let ((new-name (concat f ",S")))
      (rename-file f new-name))))

(provide 'init-mu4e)
