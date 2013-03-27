(defun setup-outline-minor-mode ()
  (local-set-key "\C-c\C-e" 'show-entry)
  (local-set-key (kbd "C-c +") 'show-entry)
  (local-set-key "\C-c\C-a" 'show-all)
  (local-set-key (kbd "C-c (") 'show-all)
  (local-set-key "\C-c\C-t" 'hide-body)
  (local-set-key (kbd "C-c )") 'hide-body)
  (local-set-key "\C-c\C-c" 'hide-entry)
  (local-set-key (kbd "C-c -") 'hide-entry))

(eval-after-load 'outline-mode
  '(add-hook 'outline-minor-mode-hook 'setup-outline-minor-mode))

(defun setup-text-mode ()
  (auto-fill-mode 1))
(add-hook 'text-mode-hook 'setup-text-mode)

(defun setup-indented-text-mode ()
  (longlines-mode 1))
(add-hook 'indented-text-mode-hook 'setup-indented-text-mode)

(defun setup-nroff-mode ()
  (setup-text-mode)
  (font-lock-mode))
(add-hook 'nroff-mode-hook 'setup-nroff-mode)

(defun setup-tex-mode ()
  (auto-fill-mode 1)
  (font-lock-mode -1)
  (setq tab-width 2)
  (setq indent-tabs-mode nil)
  (when (boundp 'font-lock-mode)
    (set-face-font 'tex-verbatim-face (face-font 'default))
    (set-face-foreground 'tex-verbatim-face "gray30")))
(add-hook 'tex-mode-hook 'setup-tex-mode)

(defun confirm-exit ()
  (y-or-n-p "Exit Emacs, Are you sure? "))
(setq kill-emacs-query-functions
      (cons 'confirm-exit kill-emacs-query-functions))

(defun setup-view-mode ()
  (local-set-key "\C-x]" 'narrow-forward-page)
  (local-set-key "\C-x[" 'narrow-backward-page))
(add-hook 'view-mode-hook 'setup-view-mode)

(add-hook 'diary-display-hook 'fancy-diary-display)
;; (add-hook 'calendar-load-hook 'mark-diary-entries)
(add-hook 'list-diary-entries-hook 'sort-diary-entries t)

(defun setup-telnet-mode ()
  (setq telnet-remote-echoes nil))
(add-hook 'telnet-mode-hook 'setup-telnet-mode)

(defun setup-dired-mode ()
  (local-set-key "k" 'dired-kill-subdir))
(add-hook 'dired-mode-hook 'setup-dired-mode)

(defun setup-comint-mode ()
  (setq comint-output-filter-functions
        (cons 'shell-strip-ctrl-m
              comint-output-filter-functions)))
(add-hook 'comint-mode-hook 'setup-comint-mode)

(add-hook 'diary-hook 'appt-make-list)

(defun setup-cvs-mode ()
  (font-lock-mode 1))

(defun cvs-load-hook ()
  (setq cvs-buffer-name-alist
	(cons `("diff" ,cvs-diff-buffer-name nil)
	      (remove-if '(lambda (x) (equal (car x) "diff"))
			 cvs-buffer-name-alist))))

(defun cvs-mode-init ()
  (add-hook 'cvs-mode-hook 'turn-on-line-truncation)
  (add-hook 'cvs-mode-hook 'setup-cvs-mode)
  (add-hook 'pcl-cvs-load-hook 'cvs-load-hook)
  (setq log-edit-keep-buffer t)
  (setenv "CVS_RSH" "ssh"))

(eval-after-load 'pcvs
  '(cvs-mode-init))

(defun setup-message-mode ()
  (setup-text-mode)
  (font-lock-mode))

(defvar *smtp-auth-info* nil)
(defun setup-smtpmail ()
  (let* ((from (message-fetch-field "from"))
         (from-address (first (rfc822-addresses from))))
    (destructuring-bind (key email password server port &key name &allow-other-keys)
        (assoc from-address *smtp-auth-info*)
      (setq user-full-name (or name user-full-name)
            user-mail-address email
            smtpmail-smtp-server server
            smtpmail-smtp-service port
            smtpmail-auth-credentials (list (list server port email password))
            smtpmail-starttls-credentials (list (list server port email nil)))
      (message "smtpmail-setup-hook: %S"
               (list :user-mail-address user-mail-address
                     :user-full-name user-full-name)))))

(defun my-compose-mail (&optional arg)
  (interactive "p")
  (let ((user-mail-address (if (> arg 1)
			       (completing-read "Send as: "
						(mapcar 'first *smtp-auth-info*))
			     user-email-address)))
    (compose-mail)))

(defun message-init ()
  (require 'starttls)
  (setq mail-user-agent 'message-user-agent
	query-user-mail-address t
	message-from-style 'angles
	message-send-mail-function 'smtpmail-send-it
	send-mail-function 'smtpmail-send-it
	message-citation-line-function 'message-insert-citation-line
	starttls-use-gnutls t
	send-mail-function 'smtpmail-send-it
	message-send-mail-function 'smtpmail-send-it
	smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
	smtpmail-auth-credentials (expand-file-name "~/.authinfo")
	smtpmail-default-smtp-server "smtp.gmail.com"
	smtpmail-smtp-server "smtp.gmail.com"
	smtpmail-smtp-service 587
	smtpmail-debug-info t)
  (global-set-key "\C-xm" 'my-compose-mail)
  (add-hook 'message-mode-hook 'setup-message-mode))

(eval-after-load 'message
  '(message-init))

(defun compilation-mode-colorize-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))

(defun setup-ansi-color ()
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'compilation-filter-hook 'compilation-mode-colorize-buffer)
  (add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply))

(eval-after-load 'ansi-color
  '(setup-ansi-color))

(eval-after-load 'w3m
  '(require 'browse-w3m))

(eval-after-load 'magit
  '(set-face-attribute 'magit-item-highlight nil
		       :background "lightgrey"
		       :foreground "black"))

(eval-after-load 'info
  '(progn
     (set-face-attribute 'info-header-node nil :foreground "black")
     (set-face-attribute 'info-node nil :foreground "black")))

(eval-after-load 'jabber
  '(setq jabber-roster-line-format "%c %n %r %s %S"
         jabber-roster-show-title nil
         jabber-show-resources nil
         jabber-chat-buffer-show-avatar nil))
