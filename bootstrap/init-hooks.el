;;;;;;;;;;;;;;;; global ;;;;;;;;;;;;;;;;

(setq inhibit-startup-message t
      inhibit-splash-screen t
      backup-inhibited t
      remote-shell-program "ssh"
      truncate-partial-width-windows t
      visible-bell 'top-bottom
      url-proxy-services nil
      compilation-scroll-output t
      transient-mark-mode t
      shell-file-name "bash"
      max-mini-window-height 1

      completion-ignored-extensions (nconc completion-ignored-extensions
                                           '(".fasl"
                                             ".dfsl"
                                             ".x86f"
                                             ".err"
                                             ".ufasl"
                                             ".DS_Store"))

      mc-gpg-path (locate-path "gpg" exec-path)
      ispell-program-name (locate-path "aspell" exec-path))

;;;;;;;;;;;;;;;; charset encoding ;;;;;;;;;;;;;;;;

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(put 'narrow-to-page 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(defun confirm-exit ()
  (y-or-n-p "Exit Emacs, Are you sure? "))

(setq kill-emacs-query-functions
      (cons 'confirm-exit kill-emacs-query-functions))

(defun setup-telnet-mode ()
  (setq telnet-remote-echoes nil))
(add-hook 'telnet-mode-hook 'setup-telnet-mode)

(defun setup-dired-mode ()
  (local-set-key "k" 'dired-kill-subdir)
  (local-set-key ">" 'dired-omit-mode)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")
        dired-omit-mode t))
(add-hook 'dired-mode-hook 'setup-dired-mode)

(defun setup-comint-mode ()
  (add-to-list 'comint-output-filter-functions 'shell-strip-ctrl-m)
  (add-to-list 'comint-output-filter-functions 'comint-truncate-buffer))

(eval-after-load 'comint
  `(setup-comint-mode))

(add-hook 'diary-hook 'appt-make-list)

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

(defun setup-edit-server ()
  (setq edit-server-default-major-mode 'normal-mode
        edit-server-new-frame nil))

(eval-after-load 'edit-server
  '(setup-edit-server))

;;;;;;;;;;;;;;;; outline ;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;; text ;;;;;;;;;;;;;;;;

(defun setup-text-mode ()
  (auto-fill-mode -1)
  (visual-line-mode))
(add-hook 'text-mode-hook 'setup-text-mode)

(defun setup-indented-text-mode ()
  (setup-text-mode))
(add-hook 'indented-text-mode-hook 'setup-indented-text-mode)

(defun setup-nroff-mode ()
  (setup-text-mode)
  (font-lock-mode))
(add-hook 'nroff-mode-hook 'setup-nroff-mode)

(defun setup-tex-mode ()
  (visual-line-mode)
  (font-lock-mode -1)
  (setq tab-width 2)
  (setq indent-tabs-mode nil)
  (when (boundp 'font-lock-mode)
    (set-face-font 'tex-verbatim-face (face-font 'default))
    (set-face-foreground 'tex-verbatim-face "gray30")))
(add-hook 'tex-mode-hook 'setup-tex-mode)

(defun setup-view-mode ()
  (local-set-key "\C-x]" 'narrow-forward-page)
  (local-set-key "\C-x[" 'narrow-backward-page))
(add-hook 'view-mode-hook 'setup-view-mode)

(eval-after-load 'adoc-mode
  '(add-to-list 'auto-mode-alist '("\\.doc$" . adoc-mode)))

;;;;;;;;;;;;;;;; version management ;;;;;;;;;;;;;;;;

(defun setup-vc-mode ()
  (setq vc-mistrust-permissions t
        vc-initial-comment t
        vc-consult-headers nil
        vc-make-backup-files t))

(eval-after-load 'vc '(setup-vc-mode))

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

(eval-after-load 'pcvs '(cvs-mode-init))

(defun setup-magit ()
  (set-face-attribute 'magit-item-highlight nil
                      :background "lightgrey"
                      :foreground "black")
  (when (facep 'magit-tag)
    (set-face-attribute 'magit-tag nil :foreground "black"))
  (setq magit-last-seen-setup-instructions "1.4.0"))

(eval-after-load 'magit '(setup-magit))

(defun alt-vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 0 name "blame" (if rev (concat  rev)))))

(eval-after-load 'vc-git
  '(fset 'vc-git-annotate-command 'alt-vc-git-annotate-command))

;;;;;;;;;;;;;;;; mail ;;;;;;;;;;;;;;;;

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

(defun setup-message ()
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

(eval-after-load 'message '(setup-message))

;;;;;;;;;;;;;;;; info ;;;;;;;;;;;;;;;;

(defun setup-info ()
  (set-face-attribute 'info-header-node nil :foreground "black")
  (set-face-attribute 'info-node nil :foreground "black")
  (require 'find-lisp)
  (let* ((local-info-directory (expand-file-name "~/.emacs.d/info"))
         (local-info-dirfile (concat local-info-directory "/dir")))
    (unless (file-directory-p local-info-directory)
      (mkdir local-info-directory))
    (mapcar (lambda (f)
              (call-process "install-info"
                            nil
                            '(" *info-setup*" t)
                            nil
                            f
                            local-info))
            (find-lisp-find-files "~/.emacs.d/el-get/" "\\.info$"))
    (add-to-list 'Info-directory-list local-info)))

(eval-after-load 'info '(setup-info))

;;;;;;;;;;;;;;;; man ;;;;;;;;;;;;;;;;

(defun setup-man ()
  (setenv "MANPATH"
          (join ":"
                '("/usr/local/share/man/"
                  "/usr/share/man/")))
  (setenv "MANWIDTH" "80")
  (add-hook 'Man-mode-hook (lambda () (setq Man-fontify-manpage nil))))
  
(eval-after-load 'man '(setup-man))

;;;;;;;;;;;;;;;; jabber ;;;;;;;;;;;;;;;;

(defun jabber-chat-html-body (xml-data who mode)
  "Print body for received message in XML-DATA."
  (let* ((message-format (caddar
                          (jabber-xml-get-children
                           (car (jabber-xml-get-children xml-data 'x))
                           'message_format)))
         (htmlp (equal "html" message-format))
         (body (car
                (jabber-xml-node-children
                 (car
                  (jabber-xml-get-children xml-data 'body))))))
    (when body
      (when (eql mode :insert)
	(if (and (> (length body) 4)
		 (string= (substring body 0 4) "/me "))
	    (let ((action (substring body 4))
		  (nick (cond
			 ((eq who :local)
			  (plist-get (fsm-get-state-data jabber-buffer-connection) :username))
			 ((or (jabber-muc-message-p xml-data)
			      (jabber-muc-private-message-p xml-data))
			  (jabber-jid-resource (jabber-xml-get-attribute xml-data 'from)))
			 (t
			  (jabber-jid-displayname (jabber-xml-get-attribute xml-data 'from))))))
	      (insert (jabber-propertize
		       (concat nick
			       " "
			       action)
		       'face 'jabber-chat-prompt-system)))
          (if htmlp
              (let ((beg (point)))
                (insert body)
                (shr-render-region beg (point)))
            (insert
             (jabber-propertize
              body
              'face (case who
                      ((:foreign :muc-foreign) 'jabber-chat-text-foreign)
                      ((:local :muc-local) 'jabber-chat-text-local)))))
          ;; (let ((beg (point))
          ;;       (text (jabber-propertize
          ;;              body
          ;;              'face (case who
          ;;                      ((:foreign :muc-foreign) 'jabber-chat-text-foreign)
          ;;                      ((:local :muc-local) 'jabber-chat-text-local)))))
          ;;   (insert text)
          ;;   (when htmlp
          ;;     (forward-line)
          ;;     (shr-render-region beg (point))))
          ))
      t)))

(defun setup-jabber ()
  (setq jabber-roster-line-format "%c %n %r %s %S"
        jabber-roster-show-title nil
        jabber-show-resources nil
        jabber-chat-buffer-show-avatar nil
        jabber-muc-autojoin nil
        jabber-chat-local-prompt-format "\n[%t] %n\n"
        jabber-chat-foreign-prompt-format "\n[%t] %n\n"
        jabber-groupchat-prompt-format "\n[%t] %n\n"
        jabber-keepalive-interval 30
        jabber-muc-disable-disco-check t)
  (require 'password-cache)
  (require 'jabber-util)
  (add-hook 'jabber-chat-mode-hook 'visual-line-mode)
  ;; (add-hook 'jabber-post-connect-hooks 'jabber-keepalive-start)
  (add-to-list 'jabber-body-printers 'jabber-chat-html-body)
  (add-to-list 'jabber-alert-message-hooks 'jabber-message-display)
  (jabber-keepalive-start))

(eval-after-load 'jabber '(setup-jabber))

;; HipChat via jabber.el
;;
;; from: https://gist.github.com/pufuwozu/4002033
(defun hipchat-join (room)
  (interactive "sRoom name: ")
  (jabber-groupchat-join (jabber-read-account)
                         (concat hipchat-number "_" room "@conf.hipchat.com")
                         hipchat-nickname
                         t))

;; Mention nicknames in a way that HipChat clients will pickup
(defun hipchat-mention (nickname)
  (interactive
   (list (jabber-muc-read-nickname jabber-group "Nickname: ")))
  (insert (concat "@\"" nickname "\" ")))

;;;;;;;;;;;;;;;; guide-key ;;;;;;;;;;;;;;;;

(defun setup-guide-key ()
  (guide-key-mode 1)
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-z" "C-c" "C-c C-d"
                                       "C-c C-x")
        guide-key/popup-window-position 'bottom))

(eval-after-load 'guide-key '(setup-guide-key))

;;;;;;;;;;;;;;;; html ;;;;;;;;;;;;;;;;

(defun setup-html-mode ()
  (visual-line-mode -1)
  (setq truncate-lines t
        truncate-partial-width-windows t))

(eval-after-load 'sgml-mode
  '(add-hook 'html-mode-hook 'setup-html-mode))

;;;;;;;;;;;;;;;; erc ;;;;;;;;;;;;;;;;

(defun erc-mode-hook ()
  (add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
  (setq erc-max-buffer-size 30000
        erc-user-full-name "kriyative"
        erc-hide-list '("JOIN" "NICK" "PART" "QUIT")
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE")
        erc-fill-function 'erc-fill-static
        erc-fill-static-center 20))

(eval-after-load 'erc
  '(add-hook 'erc-mode-hook 'erc-mode-hook))

;;;;;;;;;;;;;;;; mode-line-stats ;;;;;;;;;;;;;;;;

(defun setup-mls-battery ()
  (setq mls-battery-formatters (list
                                (cons "c" (lambda (stats)
                                            (car (nth 0 stats))))
                                (cons "r" (lambda (stats)
                                            (car (nth 1 stats))))
                                (cons "B" (lambda (stats)
                                            (car (nth 2 stats))))
                                (cons "d" (lambda (stats)
                                            (car (nth 3 stats))))
                                (cons "L" (lambda (stats)
                                            (car (nth 4 stats))))
                                (cons "p" (lambda (stats)
                                            (car (nth 5 stats))))
                                (cons "m" (lambda (stats)
                                            (car (nth 6 stats))))
                                (cons "h" (lambda (stats)
                                            (car (nth 7 stats))))
                                (cons "t" (lambda (stats)
                                            (car (nth 8 stats)))))
        mls-battery-settings '((:formats
                                ((:primary "&p{b}")
                                 (:secondary " BAT[%p{%%},%t]")
                                 (:monitor "&p")))
                               (:levels
                                (("%p" ((75.0 "norm")
                                        (35.0 "warn")
                                        (0.0  "crit"))))))))

(eval-after-load 'mls-battery '(setup-mls-battery))

(defun setup-mls ()
  (require 'mls-battery)
  (mls-battery-start)
  (define-key ctl-z-map "%" 'mls-mode-line-toggle)
  (setq mls-modules '(battery cpu memory disk misc)
        mls-format-primary '((:eval (mls-display "battery" :primary))
                             (:eval (mls-display "cpu" :primary))
                             (:eval (mls-display "memory" :primary))
                             (:eval (mls-display "disk" :primary))
                             (:eval (mls-display "misc" :primary)))
        mls-format-secondary '((:eval (mls-display "battery" :secondary))
                               (:eval (mls-display "cpu" :secondary))
                               (:eval (mls-display "memory" :secondary))
                               (:eval (mls-display "disk" :secondary))
                               (:eval (mls-display "misc" :secondary))))
  (mode-line-stats-mode 1)
  (mls-mode-line-toggle))

;; (eval-after-load 'mode-line-stats
;;   `(setup-mls))

;;;;;;;;;;;;;;;; efun ;;;;;;;;;;;;;;;;

(defun setup-efun-cmds ()
  (global-set-key "\C-x\C-f" 'x-find-file))

(eval-after-load 'efun-cmds '(setup-efun-cmds))

;;;;;;;;;;;;;;;; dictionary ;;;;;;;;;;;;;;;;

(defun dictionary-init ()
  (load-library "dictionary-init")
  ;; (setq dictionary-server "dict.org")
  (global-set-key "\C-cs" 'dictionary-search)
  (global-set-key "\C-cm" 'dictionary-match-words))

(eval-after-load 'dictionary '(dictionary-init))

;;;;;;;;;;;;;;;; ibuffer ;;;;;;;;;;;;;;;;

(eval-after-load 'ibuffer '(setq ibuffer-expert t))

;;;;;;;;;;;;;;;; calendar ;;;;;;;;;;;;;;;;

(defun iso-calendar ()
  (interactive)
  (setq european-calendar-style nil)
  (setq calendar-date-display-form
        '(year
          "-"
          (if (< (length month) 2) (concat "0" month) month)
          "-"
          (if (< (length day) 2) (concat "0" day) day)))
  (setq diary-date-forms
        '((year "-" month "-" day "[^0-9]")
          (month "/" day "[^/0-9]")
          (month "/" day "/" year "[^0-9]")
          (monthname " *" day "[^,0-9]")
          (monthname " *" day ", *" year "[^0-9]")
          (dayname "\\W")))
  (cond
   ((string-match "^2[12]" emacs-version)
    (update-calendar-mode-line))
   (t
    (when (fboundp 'calendar-update-mode-line)
      (calendar-update-mode-line)))))

(defun setup-calendar ()
  (iso-calendar)
  (add-hook 'diary-display-hook 'fancy-diary-display)
  ;; (add-hook 'calendar-load-hook 'mark-diary-entries)
  (add-hook 'list-diary-entries-hook 'sort-diary-entries t)
  (setq display-time-day-and-date t
        display-time-world-list '(("America/Los_Angeles" "Cupertino")
                                  ("America/New_York" "New York")
                                  ("Europe/London" "London")
                                  ("Europe/Paris" "Paris")
                                  ("Asia/Calcutta" "Chennai")
                                  ("Asia/Singapore" "Singapore")
                                  ("Australia/Sydney" "Sydney")
                                  ("Pacific/Auckland" "Auckland"))
        display-time-world-time-format "%a %d %b %R %Z"))

(eval-after-load 'calendar '(setup-calendar))

(defun setup-diary ()
  ;; (setq appt-message-warning-time 10)
  (add-hook 'list-diary-entries-hook 'include-other-diary-files t)
  (when (file-exists-p diary-file) (diary 0)))

(eval-after-load 'diary '(setup-diary))

;;;;;;;;;;;;;;;; bbdb ;;;;;;;;;;;;;;;;

(defun setup-bbdb ()
  (bbdb-insinuate-message)
  (bbdb-initialize 'message 'sc))

(eval-after-load 'bbdb '(setup-bbdb))

(defun setup-epa-file ()
  (epa-file-enable))

(eval-after-load 'epa-file '(setup-epa-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun setup-emms ()
  (emms-all)
  (emms-default-players)
  (setq emms-source-file-default-directory "~/Music/")
  (require 'emms-player-mpv)
  (add-to-list 'emms-player-list 'emms-player-mpv)
  (setq emms-playlist-default-major-mode 'emms-playlist-mode))

(eval-after-load 'emms-setup '(setup-emms))
