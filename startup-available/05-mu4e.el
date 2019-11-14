(my-el-get-bundles
 mu4e
 mu4e-multi
 mu4e-maildirs-extension)

;;;;;;;;;;;;;;;; mu4e

(defun mu4e-get-inbox-maildirs ()
  (remove-if-not (lambda (x)
                   (string-match "INBOX$" x))
                 (mu4e-get-maildirs)))

(defun mu4e-ask-inbox (prompt)
  "Ask the user for a shortcut (using PROMPT) as defined in
`mu4e-maildir-shortcuts', then return the corresponding folder
name. If the special shortcut 'o' (for _o_ther) is used, or if
`mu4e-maildir-shortcuts' is not defined, let user choose from all
maildirs under `mu4e-maildir'."
  (let ((prompt (mu4e-format "%s" prompt)))
    (funcall mu4e-completing-read-function
             prompt
             (mu4e-get-inbox-maildirs))))

(defun mu4e~headers-jump-to-inbox (maildir)
  "Show the messages in maildir (user is prompted to ask what
maildir)."
  (interactive
   (let ((maildir (mu4e-ask-inbox "Jump to maildir: ")))
     (list maildir)))
  (when maildir
    (mu4e-mark-handle-when-leaving)
    (mu4e-headers-search
     (format "maildir:\"%s\"" maildir))))

(defun mu4e-headers-mode-hook ()
  (setq mu4e-split-view (if (< 128 (frame-width)) 'vertical 'horizontal)
        mu4e-headers-visible-columns (/ (frame-width) 3)))

(defun mu4e-view-mode-hook ()
  (when (and (boundp 'mu4e~view-msg)
             mu4e~view-msg
             (mu4e-message-field mu4e~view-msg :body-html))
    (setq truncate-lines t))
  (setq fill-column 85
        browse-url-browser-function 'browse-url-default-browser
        shr-width nil)
  (visual-line-mode))

(defun mu4e-compose-mode-hook ()
  (setq mu4e-compose-format-flowed t
        mu4e-compose-dont-reply-to-self t
        mu4e-compose-keep-self-cc nil
        mu4e-compose-complete-addresses t
        use-hard-newlines t))

(defun mu4e-action-view-in-system-browser (msg)
  (let ((browse-url-browser-function 'browse-url-default-browser))
    (mu4e-action-view-in-browser msg)))

(use-package mu4e
  :demand t
  :config
  (require 'org-mu4e)
  (setq mu4e-maildir "~/Mail" ;; top-level Maildir
        mu4e-get-mail-command "mbsync-all -u"
        ;; mu4e-get-mail-command "/bin/true"
        mu4e-update-interval nil
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
        mu4e-sent-messages-behavior 'delete
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
        mu4e-index-cleanup t
        ;; mu4e-index-cleanup nil      ;; don't do a full cleanup check
        mu4e-index-lazy-check nil
        ;; mu4e-index-lazy-check t ;; don't consider up-to-date dirs
        org-export-with-toc nil
        mu4e-view-use-gnus nil          ;; don't use gnus to render
        gnus-inhibit-images t)
  (add-to-list 'mu4e-view-actions
               '("view in browser" . mu4e-action-view-in-system-browser))
  (add-hook 'mu4e-headers-mode-hook 'mu4e-headers-mode-hook)
  (add-hook 'mu4e-view-mode-hook 'mu4e-view-mode-hook)
  ;; (add-hook 'mu4e-compose-mode-hook 'org-mu4e-compose-org-mode)
  (add-hook 'mu4e-compose-mode-hook 'message-mode-hook)
  (add-hook 'mu4e-compose-mode-hook 'mu4e-compose-mode-hook)
  (add-to-list 'mu4e-bookmarks
               '("flag:flagged AND NOT flag:trashed"
                 "Flagged messages" 102))
  (define-key mu4e-main-mode-map "i" 'mu4e~headers-jump-to-inbox))

;; Mitigate Bug#28350 (security) in Emacs 25.2 and earlier.
(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))

(use-package mu4e-multi
  :demand t
  :bind
  (("C-x m" . mu4e-multi-compose-new))
  :config
  (when (fboundp 'setup-mu4e-multi-account)
    (setup-mu4e-multi-account))
  (setq mu4e-user-mail-address-list
        (mapcar (lambda (p)
                  (cdr (assoc 'user-mail-address (cdr p))))
                mu4e-multi-account-alist))
  (mu4e-multi-enable)
  (remove-hook 'message-mode-hook 'mu4e-multi-compose-set-account)
  (add-hook 'mu4e-compose-mode-hook 'mu4e-multi-compose-set-account)
  (add-hook 'message-send-mail-hook 'mu4e-multi-smtpmail-set-msmtp-account))

(defun my-mu4e-reload-main ()
  (interactive)
  (mu4e-maildirs-extension-force-update '(16)))

(use-package mu4e-maildirs-extension
  :config
  (mu4e-maildirs-extension)
  (setq mu4e-maildirs-extension-count-command-format
        (concat mu4e-mu-binary " find %s -u --fields 'i' | wc -l")
        mu4e-maildirs-extension-maildir-format-spec
        (lambda(m)
          (list (cons ?i (plist-get m :indent))
                (cons ?p (plist-get m :prefix))
                (cons ?l (plist-get m :level))
                (cons ?e (plist-get m :expand))
                (cons ?P (plist-get m :path))
                (cons ?n (plist-get m :name))
                (cons ?u (or (add-number-grouping (plist-get m :unread)) ""))
                (cons ?t (or (add-number-grouping (plist-get m :total)) "")))))
  (define-key mu4e-main-mode-map "g" 'my-mu4e-reload-main))

(defun message-mode-hook ()
  (setq message-fill-column nil
        message-from-style 'angles
        message-citation-line-function 'message-insert-citation-line
        message-cite-style 'message-cite-style-gmail
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

(use-package mu4e-conversation
  :disabled t
  :config
  ;; (setq mu4e-view-func 'mu4e~headers-view-handler)
  (setq mu4e-view-func 'mu4e-conversation))

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
                         (mu4e~proc-move docid
                                         (mu4e~mark-check-target target) "-N")))
            (remove-if (lambda (x)
                         (equal 'trash (car x)))
                       mu4e-marks)))

(defun mu4e-maildirs-extension-index-updated-handler ()
  "Handler for `mu4e-index-updated-hook'."
  (mu4e-maildirs-extension-force-update '(16)))
