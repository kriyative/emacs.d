;;;;;;;;;;;;;;;; global ;;;;;;;;;;;;;;;;

(load-file-if-exists "~/.personal.el")

(when (fboundp 'init-deps)
  (init-deps))

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

(add-to-list 'display-buffer-alist
             '("\\*Completions\\*" vertical-display-completions))

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

;;;;;;;;;;;;;;;; hooks ;;;;;;;;;;;;;;;;

(defun confirm-exit ()
  (y-or-n-p "Exit Emacs, Are you sure? "))

(add-to-list 'kill-emacs-query-functions 'confirm-exit)

(defun post-startup-hook ()
  (when (fboundp 'global-auto-complete-mode)
    (global-auto-complete-mode -1)))

(add-hook 'emacs-startup-hook 'post-startup-hook)

(use-package
    ibuffer
  :config (setq ibuffer-expert t))

(use-package
    vc
  :config (setq vc-mistrust-permissions t
                vc-initial-comment t
                vc-consult-headers nil
                vc-make-backup-files t))

(use-package
    comint
  :config (progn
            (add-to-list 'comint-output-filter-functions 'shell-strip-ctrl-m)
            (add-to-list 'comint-output-filter-functions 'comint-truncate-buffer)))

(use-package
    telnet
  :config (setq telnet-remote-echoes nil))

(use-package
    dired-x
  :demand t
  :bind (:map dired-mode-map
              ("k" . dired-kill-subdir)
              (">" . dired-omit-mode)
              ([C-return] . dired-open-file))
  :config
  (set-default 'dired-omit-mode t)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")
	dired-listing-switches "-alh"))

(unless (fboundp 'org-at-planning-p)
  (defun org-at-planning-p ()
    nil))

(defun setup-diary ()
  (add-hook 'list-diary-entries-hook 'include-other-diary-files t)
  (when (file-exists-p diary-file) (diary 0))
  (add-hook 'diary-hook 'appt-make-list)
  (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
  (add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files))

(use-package diary-lib :config (setup-diary))

(defun setup-ansi-color ()
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'compilation-filter-hook 'compilation-mode-colorize-buffer)
  (add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply))

(use-package ansi-color :config (setup-ansi-color))

(use-package
    edit-server
  :config (setq edit-server-default-major-mode 'normal-mode
                edit-server-new-frame nil))

(use-package
    outline-mode
  :bind (("\C-c\C-e" . show-entry)
         ("C-c +"    . show-entry)
         ("\C-c["    . show-entry)
         ("\C-c\C-a" . show-all)
         ("C-c ("    . show-all)
         ("\C-c{"    . show-all)
         ("\C-c\C-t" . hide-body)
         ("\C-c}"    . hide-body)
         ("C-c )"    . hide-body)
         ("\C-c\C-c" . hide-entry)
         ("C-c -"    . hide-entry)
         ("\C-c]"    . hide-entry))
  :config (add-hook 'outline-minor-mode-hook 'setup-outline-minor-mode))

;;;;;;;;;;;;;;;; setup various modes ;;;;;;;;;;;;;;;;

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

;;; ascii-doc mode
;;;
;; (use-package
;;  adoc-mode
;;  :config (add-to-list 'auto-mode-alist '("\\.doc$" . adoc-mode)))

(defun setup-vc ()
  (setq vc-mistrust-permissions t
        vc-initial-comment t
        vc-consult-headers nil
        vc-make-backup-files t))

(use-package vc :config (setup-vc))

(defun setup-cvs-mode ()
  (font-lock-mode 1))

(defun cvs-load-hook ()
  (setq cvs-buffer-name-alist
	(cons `("diff" ,cvs-diff-buffer-name nil)
	      (remove-if '(lambda (x) (equal (car x) "diff"))
			 cvs-buffer-name-alist))))

(defun setup-cvs ()
  (add-hook 'cvs-mode-hook 'turn-on-line-truncation)
  (add-hook 'cvs-mode-hook 'setup-cvs-mode)
  (add-hook 'pcl-cvs-load-hook 'cvs-load-hook)
  (setq log-edit-keep-buffer t)
  (setenv "CVS_RSH" "ssh"))

(use-package pcvs :config (setup-cvs))

(defun magit-setup-hook ()
  (local-unset-key [C-tab])
  (define-key magit-mode-map [C-tab] nil))

(defun setup-magit ()
  (when (facep 'magit-item-highlight)
    (set-face-attribute 'magit-item-highlight nil
                        :background "lightgrey"
                        :foreground "black"))
  (when (facep 'magit-tag)
    (set-face-attribute 'magit-tag nil :foreground "black"))
  (setq magit-last-seen-setup-instructions "1.4.0")
  (add-hook 'magit-mode-hook 'magit-setup-hook))

(use-package magit :config (setup-magit))

(use-package magit-gh-pulls
  :config (add-hook 'magit-mode-hook 'magit-gh-pulls-mode))

(defun alt-vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 0 name "blame" (if rev (concat  rev)))))

(use-package
    vc-git
  :config (fset 'vc-git-annotate-command 'alt-vc-git-annotate-command))

(defun add-el-get-info-dirs ()
  (require 'find-lisp)
  (let ((local-info-directory (expand-file-name "~/.emacs.d/info")))
    (unless (file-directory-p local-info-directory)
      (mkdir local-info-directory))
    (with-cwd local-info-directory
      (dolist (f (find-lisp-find-files "~/.emacs.d/el-get/" "\\.info$"))
	(let ((d (file-name-directory f)))
	  (when (directory-files d nil "\\.info$")
	    (call-process "install-info"
			  nil
			  '(" *info-setup*" t)
			  nil
			  "--debug"
			  f
			  "dir")
	    (add-to-list 'Info-additional-directory-list d)))))
    (add-to-list 'Info-directory-list local-info-directory))
  (add-to-list 'Info-directory-list "/app/stumpwm/share/info")
  (add-to-list 'Info-directory-list "/usr/local/share/info"))

(defun setup-info ()
  (set-face-attribute 'info-header-node nil :foreground "black")
  (set-face-attribute 'info-node nil :foreground "black")
  (add-el-get-info-dirs))

(use-package info :config (setup-info))

(defun setup-man ()
  (setenv "MANPATH"
          (join ":"
                '("/usr/local/share/man/"
                  "/usr/share/man/")))
  (setenv "MANWIDTH" "80")
  (setq Man-fontify-manpage nil))

(use-package man :config (setup-man))

(defun setup-guide-key ()
  (guide-key-mode 1)
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-z"
                                       "C-c" "C-c C-d" "C-c C-x"
                                       "C-c p")
        guide-key/popup-window-position 'bottom))

(use-package guide-key :config (setup-guide-key))

(defun setup-html-mode ()
  (visual-line-mode -1)
  (setq truncate-lines t
        truncate-partial-width-windows t))

(use-package
    sgml-mode
  :config (add-hook 'html-mode-hook 'setup-html-mode))

(defun setup-erc ()
  (add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
  (setq erc-max-buffer-size 30000
        erc-hide-list '("JOIN" "NICK" "PART" "QUIT")
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE")
        erc-fill-function 'erc-fill-static
        erc-fill-static-center 20))

(use-package erc :config (setup-erc))

(use-package
    efun-cmds
  :bind (("\C-x\C-f" . x-find-file)))

(use-package
    dictionary
  :bind (("\C-cs" . dictionary-search)
         ("\C-cm" . dictionary-match-words))
  :config (load-library "dictionary-init"))

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
  (setq display-time-day-and-date nil
        display-time-world-list '(("Pacific/Honolulu" "Honolulu")
				  ("America/Anchorage" "Anchorage")
				  ("America/Los_Angeles" "Los Angeles")
				  ("America/Phoenix" "Phoenix")
				  ("America/Chicago" "Chicago")
                                  ("America/New_York" "New York")
                                  ("Europe/London" "London")
                                  ("Europe/Paris" "Paris")
                                  ("Asia/Calcutta" "Calcutta")
                                  ("Asia/Singapore" "Singapore")
                                  ("Australia/Sydney" "Sydney")
                                  ("Pacific/Auckland" "Auckland"))
        display-time-world-time-format "%a %d %b %R %Z"))

(use-package calendar :config (setup-calendar))

(defun setup-bbdb ()
  (bbdb-insinuate-message)
  (bbdb-initialize 'message 'mu4e)
  (setq bbdb-mail-user-agent 'message-user-agent
        bbdb-mua-pop-up nil))

(use-package bbdb :config (setup-bbdb))

(defun setup-epa-file ()
  (epa-file-enable))

(use-package epa-file :config (setup-epa-file))

(defun setup-emms ()
  (add-to-list 'emms-player-base-format-list "opus")
  (emms-all)
  (emms-default-players)
  (setq emms-source-file-default-directory "~/Music/"
	emms-player-mplayer-parameters '("-slave" "-quiet" "-really-quiet" "-vo" "null"))
  ;; (require 'emms-player-mpv)
  ;; (add-to-list 'emms-player-list 'emms-player-mpv)
  (setq emms-playlist-default-major-mode 'emms-playlist-mode))

(add-hook 'emms-info-functions 'emms-info-track-description)

(use-package emms :config (setup-emms))

;;;;;;;;;;;;;;;; org ;;;;;;;;;;;;;;;;

(defvar org-journal-date-format "%Y-%m-%d %H:%M:%S"
  "Date format string for journal headings.")

(defun org-journal-entry ()
  "Create a new diary entry for today or append to an existing one."
  (interactive)
  (switch-to-buffer (find-file org-journal-file))
  (widen)
  (let ((today (format-time-string org-journal-date-format)))
    (beginning-of-buffer)
    (next-line)
    (org-insert-heading)
    (insert today)
    (insert "\n\n\n")
    (backward-char)
    (unless (= 2 (current-column))
      (insert "  "))))

(defun my-org-time-stamp-inactive ()
  (interactive)
  (org-time-stamp-inactive (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun my-org-mode-hook ()
  (visual-line-mode)
  (org-display-inline-images))

(defun setup-org ()
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'my-org-mode-hook)

  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (global-set-key (kbd "C-c j") 'org-journal-entry)
  (define-key org-mode-map "\C-c!" 'my-org-time-stamp-inactive)
  (define-key org-mode-map (kbd "C-c (") 'show-all)
  (define-key org-mode-map (kbd "C-c )") 'hide-sublevels)
  (define-key org-mode-map '[C-tab] nil)
  (define-key org-mode-map "\M-n" 'next-page)
  (define-key org-mode-map "\M-p" 'prev-page)
  (define-key org-mode-map (kbd "C-c o") 'org-open-at-point)
  (setq org-export-html-postamble nil
	org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")
  (org-babel-do-load-languages 'org-babel-load-languages '((ditaa . t))))

(use-package org :config (setup-org))

(use-package org-agenda
  :config
  (setq org-agenda-include-diary t
        org-agenda-prefix-format (cons
                                  '(agenda . " %i %-16:c%?-12t% s")
                                  (remove-if (lambda (x)
                                               (eq (car x) 'agenda))
                                             org-agenda-prefix-format))))

(defun setup-org-passwords ()
  (setq org-passwords-file "~/.pwcrypt.gpg"
        org-passwords-random-words-dictionary "/etc/dictionaries-common/words"))

(use-package org-passwords
  :if (memq 'org-passwords enable-features)
  :config (setup-org-passwords))

(require 'org)
(use-package org-gcal
  :if (memq 'org-gcal enable-features)
  :config
  (setq org-gcal-client-id my-org-gcal-client-id
        org-gcal-client-secret my-org-gcal-client-secret
        org-gcal-file-alist my-org-gcal-file-alist)
  (setq org-agenda-mode-hook nil)
  (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync)))
  (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync)))
  )

(use-package org-blog)

;;;;;;;;;;;;;;;; logging ;;;;;;;;;;;;;;;;

;; (defun setup-log4e ()
;;   (log4e:deflogger "hoge" "%t [%l] %m" "%H:%M:%S"))

;; (use-package log4e
;;   :config (setup-log4e))

;;;;;;;;;;;;;;;; mail ;;;;;;;;;;;;;;;;

(defun mu4e-headers-mode-hook ()
  (setq mu4e-split-view (if (< 128 (frame-width)) 'vertical 'horizontal)
	mu4e-headers-visible-columns (/ (frame-width) 3)))

(defun mu4e-view-mode-hook ()
  ;; (if (mu4e-message-field mu4e~view-msg :body-html)
  ;;     (setq truncate-lines t)
  ;;   (visual-line-mode))
  (setq fill-column 132
	browse-url-browser-function 'browse-url-default-browser
	shr-width nil
	mu4e-compose-format-flowed t
	use-hard-newlines nil))

(defun mu4e-action-view-in-system-browser (msg)
  (let ((browse-url-browser-function 'browse-url-default-browser))
    (mu4e-action-view-in-browser msg)))

(defun setup-mu4e ()
  (require 'org-mu4e)
  (setq mu4e-maildir "~/Mail" ;; top-level Maildir
	mu4e-get-mail-command "mbsync-all"
	;; mu4e-get-mail-command "true"
        mu4e-update-interval 300
        ;; fix for duplicate UID per:
        ;; http://pragmaticemacs.com/emacs/fixing-duplicate-uid-errors-when-using-mbsync-and-mu4e/
        mu4e-change-filenames-when-moving t
        mu4e-headers-skip-duplicates t
        mu4e-view-prefer-html t
        mu4e-hide-index-messages t
        mu4e-split-view 'vertical
        mu4e-headers-fields '((:human-date .  12) ;; alternatively, use :human-date
                              ;; (:flags      .   6)
                              (:from       .  24)
                              (:subject    .  nil))
        mu4e-sent-messages-behavior 'delete
        mu4e-view-show-addresses t
        mu4e-view-mode-hook '(bbdb-mua-auto-update)
        org-mu4e-convert-to-html t
        message-sendmail-f-is-evil 't
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "msmtp"
        mu4e-compose-dont-reply-to-self t
        mu4e-compose-keep-self-cc nil
        message-kill-buffer-on-exit t
        mu4e-headers-leave-behavior 'apply
        mu4e-compose-format-flowed t
	mu4e-html2text-command 'mu4e-shr2text
        ;; mu4e-html2text-command "html2text -utf8 -width 72"
	mu4e-doc-dir mu4e-builddir
        org-export-with-toc nil)
  (add-to-list 'mu4e-view-actions '("view in browser" . mu4e-action-view-in-system-browser))
  (add-hook 'mu4e-headers-mode-hook 'mu4e-headers-mode-hook)
  (add-hook 'mu4e-view-mode-hook 'mu4e-view-mode-hook)
  (add-hook 'mu4e-compose-mode-hook 'org-mu4e-compose-org-mode)
  (add-hook 'mu4e-compose-mode-hook 'message-mode-hook)
  (add-to-list 'mu4e-bookmarks
	       '("flag:flagged AND NOT flag:trashed" "Flagged messages" 102))
  (define-key mu4e-main-mode-map "i" 'mu4e~headers-jump-to-inbox))

(use-package mu4e
  :demand t
  :config (setup-mu4e))

;; Mitigate Bug#28350 (security) in Emacs 25.2 and earlier.
(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))

(use-package mu4e-multi
  :demand t
  :bind (("C-x m" . mu4e-multi-compose-new))
  :config (progn
            (when (fboundp 'setup-mu4e-multi-account)
              (setup-mu4e-multi-account))
            (setq mu4e-user-mail-address-list
                  (mapcar (lambda (p)
                            (cdr (assoc 'user-mail-address (cdr p))))
                          mu4e-multi-account-alist))
            (mu4e-multi-enable)
            (remove-hook 'message-mode-hook 'mu4e-multi-compose-set-account)
            (add-hook 'mu4e-compose-mode-hook 'mu4e-multi-compose-set-account)
            (add-hook 'message-send-mail-hook 'mu4e-multi-smtpmail-set-msmtp-account)))

(use-package mu4e-maildirs-extension
  :config (progn
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
			  (cons ?t (or (add-number-grouping (plist-get m :total)) "")))))))

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
  :config (add-hook 'message-mode-hook 'message-mode-hook))

(defvar window-configuration-stack nil)

(defun push-window-configuration ()
  (push (current-window-configuration) window-configuration-stack))

(defun pop-window-configuration ()
  (pop window-configuration-stack))

(defun switch-to-mu4e ()
  (interactive)
  (push-window-configuration)
  (delete-other-windows)
  (switch-to-buffer "*scratch*")
  (mu4e))

(defun switch-back ()
  (interactive)
  (let ((window-conf (pop-window-configuration)))
    (when window-conf
      (set-window-configuration window-conf))))

(defvar *switch-to-apps* nil)

(defun switch-to-app (app)
  (interactive
   (list
    (completing-read "Switch to: "
                     (sort (mapcar 'car *switch-to-apps*) 'string-lessp))))
  (message app)
  (push-window-configuration)
  (delete-other-windows)
  (let* ((app (intern app))
         (handler (cdr (assq app *switch-to-apps*))))
    (funcall handler)))

(defun switch-to-emms ()
  (interactive)
  (push-window-configuration)
  (delete-other-windows)
  (emms-playlist-mode-go))

(add-to-list '*switch-to-apps* '(mu4e . mu4e))
(add-to-list '*switch-to-apps* '(emms . emms-playlist-mode-go))

;;;;;;;;;;;;;;;; projectile ;;;;;;;;;;;;;;;;

;; (use-package projectile :config (projectile-global-mode))
;; (projectile-global-mode -1)

;;;;;;;;;;;;;;;; lisp ;;;;;;;;;;;;;;;;

(defun setup-lisp-indent-function (&optional indent-function)
  (let ((indent-function (or indent-function 'lisp-indent-function))
        (lisp-indent-alist '((awhen . 1)
                             (when-let . 1)
                             (aif . if)
                             (if-let . if)
                             (awhile . 1)
                             (while-let . 1)
                             (bind . 1)
                             (callback . lambda)
                             (c-fficall . with-slots)
                             (with-cwd . 1)
                             (save-values . 1))))
    (dolist (x lisp-indent-alist)
      (put (car x)
           indent-function
           (if (numberp (cdr x))
               (cdr x)
             (get (cdr x) indent-function))))))

(defun set-common-lisp-block-comment-syntax ()
  (modify-syntax-entry ?# "<1" font-lock-syntax-table)
  (modify-syntax-entry ?| ">2" font-lock-syntax-table)
  (modify-syntax-entry ?| "<3" font-lock-syntax-table)
  (modify-syntax-entry ?# ">4" font-lock-syntax-table))

(defun my-common-lisp-mode-hook ()
  (font-lock-mode)
  (font-lock-add-keywords 'lisp-mode
			  '(("defclass\*" . font-lock-keyword-face)))
  (modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)
  (set (make-local-variable 'lisp-indent-function) 'common-lisp-indent-function)
  (setup-lisp-indent-function 'common-lisp-indent-function)
  (setq indent-tabs-mode nil))

(defun lispdoc ()
  "Searches lispdoc.com for SYMBOL, which is by default the symbol
currently under the curser"
  (interactive)
  (let* ((word-at-point (word-at-point))
	 (symbol-at-point (symbol-at-point))
	 (default (symbol-name symbol-at-point))
	 (inp (read-from-minibuffer
	       (if (or word-at-point symbol-at-point)
		   (concat "Symbol (default " default "): ")
		 "Symbol (no default): "))))
    (if (and (string= inp "")
	     (not word-at-point)
	     (not symbol-at-point))
	(message "you didn't enter a symbol!")
      (let ((search-type (read-from-minibuffer
			  "full-text (f) or basic (b) search (default b)? ")))
	(browse-url (concat "http://lispdoc.com?q="
			    (if (string= inp "")
				default
			      inp)
			    "&search;="
			    (if (string-equal search-type "f")
				"full+text+search"
			      "basic+search")))))))

(defun my-slime-list-connections ()
  (interactive)
  (slime-list-connections)
  (pop-to-buffer "*SLIME Connections*"))

(defun my-url-browser-function (&rest args)
  (apply (if current-prefix-arg
	     'browse-url-default-browser
	   'w3m-browse-url-other-window)
	 args))

(defun my-slime-mode-hook ()
  (setq common-lisp-hyperspec-root
	"file:///opt/cl-doc/HyperSpec/"
	;; "http://www.lispworks.com/reference/HyperSpec/"
	browse-url-browser-function 'my-url-browser-function)
  ;; (set-face-attribute 'slime-highlight-edits-face nil :background "grey")
  (define-key slime-mode-map "\M-\C-x" 'slime-compile-defun)
  (define-key slime-mode-map "\C-c\C-xc" 'my-slime-list-connections)
  (unless (boundp 'last-command-char)
    (defvar last-command-char nil)))

(defun slime-mode-init ()
  (slime-setup '(slime-repl))
  (setq slime-protocol-version 'ignore)
  (add-hook 'slime-mode-hook 'my-slime-mode-hook))

(use-package slime :config (slime-mode-init))

(defun sbcl ()
  (interactive)
  (if-bind (sbcl-path (locate-path "sbcl" exec-path))
    (let ((slime-lisp-implementations `((sbcl (,sbcl-path)))))
      (setenv "SBCL_HOME" (file-name-directory sbcl-path))
      (slime))
    (message "The sbcl application could not be found")))

(defun ccl ()
  (interactive)
  (if-bind (ccl-path (locate-path "ccl64" exec-path))
    (let ((slime-lisp-implementations `((ccl (,ccl-path)))))
      (slime))
    (message "The ccl application could not be found")))

(defun clisp ()
  (interactive)
  (if-bind (clisp-path (locate-path "clisp" exec-path))
    (let ((slime-lisp-implementations `((clisp (,clisp-path " -K full")))))
      (slime))
    (message "The clisp application could not be found")))

(defun ecl ()
  (interactive)
  (if-bind (ecl-path (locate-path "ecl.sh" exec-path))
    (let ((slime-lisp-implementations `((ecl (,ecl-path)))))
      (slime))
    (message "The ecl application could not be found")))

;;;;;;;;;;;;;;;; emacs lisp ;;;;;;;;;;;;;;;;

(defun my-emacs-lisp-mode-hook ()
  ;; (local-set-key "	" 'lisp-complete-symbol)
  (outline-minor-mode 1)
  (setq outline-regexp "^[(;]"
	indent-tabs-mode nil)
  (setup-lisp-indent-function)
  (local-set-key "\M-." 'find-function)
  (font-lock-mode 1)
  (auto-complete-mode -1)
  (eldoc-mode 1))

(defun lisp-mode-init ()
  (add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
  (add-hook 'lisp-interaction-mode-hook 'my-emacs-lisp-mode-hook)
  (add-hook 'lisp-mode-hook 'my-common-lisp-mode-hook)
  (add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode)))

(use-package lisp-mode :config (lisp-mode-init))

;;;;;;;;;;;;;;;; clojure ;;;;;;;;;;;;;;;;

(defun clojure-mode-hook ()
  (auto-revert-mode 1)
  (outline-minor-mode 1))

(defun setup-clojure ()
  (add-hook 'clojure-mode-hook 'clojure-mode-hook)
  (add-to-list 'auto-mode-alist '("\\.cljc\\'" . clojure-mode)))

(use-package clojure-mode :config (setup-clojure))

(defun setup-cider-repl ()
  (cider-repl-add-shortcut "sayoonara" 'cider-quit)
  (setq cider-repl-use-pretty-printing nil))

(use-package cider-repl
  :init (setq cider-completion-use-context nil
	      cider-prompt-for-symbol nil
	      cider-repl-display-help-banner nil
	      cider-use-overlays nil
	      cider-repl-use-pretty-printing t)
  :bind (("\C-c\M-o" . cider-repl-clear-buffer))
  :config (setup-cider-repl))

(defun cider-mode-hook ()
  (eldoc-mode)
  (outline-minor-mode)
  (define-key cider-mode-map "\C-c\C-k" 'cider-load-buffer-ext)
  (define-key cider-mode-map "\C-c," 'cider-test-run-loaded-tests)
  (define-key cider-mode-map "\C-c\M-," 'cider-test-run-test))

;; (remove-hook 'cider-mode-hook 'cider-mode-hook)

(defun setup-cider ()
  (add-hook 'cider-mode-hook 'cider-mode-hook)
  (setq cider-lein-parameters "trampoline repl :headless"))

(use-package cider :config (setup-cider))

(defun cider-remove-current-ns (&optional buffer)
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (cider-tooling-eval
     (format "(remove-ns '%s)" (cider-current-ns))
     (cider-interactive-eval-handler (current-buffer)))))

(defun cider-load-buffer-ext (&optional arg)
  (interactive "p")
  (if (< 1 arg)
      (progn
        (message "Removing namespace: %s" (cider-current-ns))
        (cider-remove-current-ns)
        (cider-load-buffer))
    (cider-load-buffer)))

;;;;;;;;;;;;;;;; c mode ;;;;;;;;;;;;;;;;

(defun c-mode-hook ()
  (setq tab-width 8
	c-basic-offset 4
	indent-tabs-mode nil)
  (font-lock-mode 1))

(defun objc-mode-setup-hook ()
  (setq tab-width 8
	c-basic-offset 4
	indent-tabs-mode nil
	truncate-lines t))

;;;;;;;;;;;;;;;; java mode ;;;;;;;;;;;;;;;;

(defun java-beginning-of-method ()
  (when (re-search-backward (concat "^\\(  \\|\t\\)"
				    "\\(synchronized"
				    "\\|public"
				    "\\|protected"
				    "\\|private\\)")
			    nil
			    'move
			    1)
    (goto-char (1- (match-end 0)))))

(defun java-end-of-method ()
  (let ((result (re-search-forward "^  }" nil t)))
    (end-of-line)
    (and result t)))

(defun java-mode-hook ()
  (setq c-basic-offset 4
	tab-width 4
	c-set-style "linux"
	indent-tabs-mode nil)
  (when (boundp 'outline-regexp)
    (outline-minor-mode 1)
    (setq outline-regexp (concat "^\\(  [  ]*\\|\\)"
                                 "\\(synchronized"
                                 "\\|public"
                                 "\\|protected"
                                 "\\|private"
                                 ;; "\\|//"
                                 "\\)")))
  (font-lock-mode 1))

(defun java-mode-init ()
  (add-hook 'java-mode-hook 'java-mode-hook)
  (require 'compile)
  (setq compilation-error-regexp-alist
        (append
         (list
          ;; works for jikes
          '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):\\([0-9]+\\):[0-9]+:[0-9]+:"
            1 2 3)
          ;; works for javac
          '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):" 1 2)

          ;; clojure.test
          '("^FAIL in (.*) (\\([^:]*\\):\\([0-9]+\\)" 1 2)
          '("^ERROR in (.*) (\\([^:]*\\):\\([0-9]+\\)" 1 2))
         compilation-error-regexp-alist)))

(use-package
    cc-mode
  :config (progn
            (add-hook 'c-mode-hook 'c-mode-hook)
            (add-hook 'objc-mode-hook 'objc-mode-setup-hook)
            (java-mode-init)))

;;;;;;;;;;;;;;;; js2-mode ;;;;;;;;;;;;;;;;

(defun my-js2-mode-hook ()
  (setq indent-tabs-mode nil))

(defun setup-js2-mode ()
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook 'my-js2-mode-hook))

(use-package js2-mode :config (setup-js2-mode))

;;;;;;;;;;;;;;;; python ;;;;;;;;;;;;;;;;

(defun setup-python ()
  (setq python-remove-cwd-from-path nil)
  (setq jedi:setup-keys t)
  (add-hook 'python-mode-hook 'jedi:setup))

;; (use-package python :config (setup-python))
(use-package jedi :config (setup-python))

(defun alert--log-clear-log ()
  (let ((buf (get-buffer "*Alerts*")))
    (when (bufferp buf)
      (with-current-buffer buf
	(delete-region (point-min) (point-max))))))

(use-package alert
  :config
  (setq alert-log-messages t))

;; (alert "Sending an alert" :title "Test")

(use-package graphviz-dot-mode
  :config (setq graphviz-dot-view-command "xdot %s"))

(use-package wttrin)

;; (use-package emacs-jabber)

(use-package git-link)

(use-package pdf-tools
  :if (memq 'pdf-tools enable-features)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (bind-keys :map pdf-view-mode-map
	     ("<s-spc>" .  pdf-view-scroll-down-or-previous-page)
	     ("g"  . pdf-view-first-page)
	     ("G"  . pdf-view-last-page)
	     ("l"  . image-forward-hscroll)
	     ("h"  . image-backward-hscroll)
	     ("j"  . pdf-view-next-page)
	     ("k"  . pdf-view-previous-page)
	     ("e"  . pdf-view-goto-page)
	     ("u"  . pdf-view-revert-buffer)
	     ("al" . pdf-annot-list-annotations)
	     ("ad" . pdf-annot-delete)
	     ("aa" . pdf-annot-attachment-dired)
	     ("am" . pdf-annot-add-markup-annotation)
	     ("at" . pdf-annot-add-text-annotation)
	     ("y"  . pdf-view-kill-ring-save)
	     ("i"  . pdf-misc-display-metadata)
	     ("s"  . pdf-occur)
	     ("b"  . pdf-view-set-slice-from-bounding-box)
	     ("r"  . pdf-view-reset-slice)))

(use-package ediff
  :config (set 'ediff-window-setup-function 'ediff-setup-windows-plain))

(defun my-midnight-hook ()
  (org-gcal-multi-fetch)
  (org-agenda-list nil nil 'day))

(use-package midnight
  :config
  (midnight-delay-set 'midnight-delay "9:00am")
  (add-hook 'midnight-hook 'my-midnight-hook))

(use-package image
  :bind (:map image-mode-map
              ("w" . image-transform-fit-to-width)
              ("h" . image-transform-fit-to-height)
              ("s" .  image-transform-set-scale)))

(defun setup-geiser ()
  ;; (geiser-set-scheme* 'chez)
  )

(use-package geiser
  :if (memq 'geiser enable-features)
  :config (add-hook 'geiser-mode-hook 'setup-geiser))

(use-package csv-mode
  :config
  (setq csv-align-style 'auto))

(use-package org-sync
  :config
  (load "os-github"))

(global-unset-key "\C-z")

(defvar ctlx-ctlj-map (make-sparse-keymap))
(let ((k "\C-x\C-j"))
  (define-prefix-command 'ctlx-ctlj-prefix 'ctlx-ctlj-map k)
  (global-set-key k 'ctlx-ctlj-prefix))

(use-package window-numbering
  :config
  (dotimes (i 10)
    (define-key ctlx-ctlj-map
      (prin1-to-string i)
      (intern (concat "select-window-" (prin1-to-string i)))))
  (window-numbering-mode)
  (window-numbering-update))

(use-package diminish
  :config
  (dolist (m '(eldoc-mode
               guide-key-mode
               auto-revert-mode
               outline-minor-mode
               ;; org-indent-mode
               visual-line-mode
               overwrite-mode))
    (diminish m)))

(use-package epa
  :config
  (setq epa-pinentry-mode 'loopback))

;;;;;;;;;;;;;;;; keys ;;;;;;;;;;;;;;;;

(global-unset-key "\M-g")
(global-set-key "\M-g" 'goto-line)
(global-set-key "\r" 'newline-and-indent)
(global-set-key "\C-xn" 'my-next-window)
(global-set-key "\C-xp" 'my-previous-window)
(define-key minibuffer-local-completion-map '[tab] 'minibuffer-complete)
(define-key minibuffer-local-completion-map '[spc] 'minibuffer-complete-word)
(define-key minibuffer-local-must-match-map '[tab] 'minibuffer-complete)
(define-key minibuffer-local-must-match-map '[spc] 'minibuffer-complete-word)
(global-set-key "\C-ct" 'transpose-lines)
(global-set-key "\C-\M-l" 'my-other-buffer)
(global-unset-key "\C-\\")
(global-set-key "\C-\\" 'compile)
(global-set-key "\C-x\C-b" 'ibuffer)
;; (global-set-key "\C-xb" 'iswitchb-buffer)
;; (global-set-key "\C-xb" 'switch-to-buffer)
(global-set-key [C-M-left] 'previous-buffer)
(global-set-key [C-M-right] 'next-buffer)
(global-set-key [?\C-.] 'tags-search)
(global-set-key [?\C-,] 'tags-loop-continue)
(global-set-key "\C-x\C-f" 'x-find-file)

(define-key ctl-x-4-map "k" 'other-window-send-keys)

;;;;;;;;;;;;;;;; ctrl-z keymap ;;;;;;;;;;;;;;;;

(defun ctl-semicolon-help ()
  (interactive)
  (message "Welcome to Ctl-;"))

(defvar ctl-semicolon-map (make-sparse-keymap))

(let ((k (kbd "C-;")))
  (global-unset-key k)
  (define-prefix-command 'ctl-semicolon-prefix 'ctl-semicolon-map k)
  (define-key global-map k 'ctl-semicolon-prefix))

(define-key ctl-semicolon-map "." 'find-tag)
(define-key ctl-semicolon-map "0" 'go-home)
(define-key ctl-semicolon-map "2" '2col-view)
(define-key ctl-semicolon-map "3" '3col-view)
(define-key ctl-semicolon-map "4" '4col-view)
(define-key ctl-semicolon-map "9" 'fill-vertical-panes)
(define-key ctl-semicolon-map "<" 'pop-tag-mark)
(define-key ctl-semicolon-map "\C-b" 'winner-undo)
(define-key ctl-semicolon-map "\C-f" 'winner-redo)
(define-key ctl-semicolon-map "\C-l" 'bury-buffer)
(define-key ctl-semicolon-map "e" 'switch-to-emms)
(define-key ctl-semicolon-map "g" 'toggle-debug-on-error)
(define-key ctl-semicolon-map "j" 'jump-to-register)
(define-key ctl-semicolon-map "l" 'cider-jack-in)
(define-key ctl-semicolon-map "m" 'switch-to-mu4e)
(define-key ctl-semicolon-map "o" 'browse-url-default-browser)
(define-key ctl-semicolon-map "q" 'switch-back)
(define-key ctl-semicolon-map "r" 'cider-switch-to-current-repl-buffer)
(define-key ctl-semicolon-map "t" 'toggle-truncate-lines)
(define-key ctl-semicolon-map "u" 'browse-url)
(define-key ctl-semicolon-map "v" 'magit-status)
(define-key ctl-semicolon-map "w" 'window-configuration-to-register)
(define-key ctl-semicolon-map "W" 'visual-line-mode)
(define-key ctl-semicolon-map "z" 'switch-to-app)
(define-key ctl-semicolon-map "\C-z" 'switch-to-app)
(define-key ctl-semicolon-map "|" 'toggle-window-split)
(define-key ctl-semicolon-map [left]  'buf-move-left)
(define-key ctl-semicolon-map [right] 'buf-move-right)
(define-key ctl-semicolon-map [down]  'buf-move-down)
(define-key ctl-semicolon-map [up]    'buf-move-up)
(define-key ctl-semicolon-map "p" 'emms-pause)

;; (define-key dired-mode-map [C-return] 'dired-open-file)

;;;;;;;;;;;;;;;; run at startup ;;;;;;;;;;;;;;;;

(display-time-mode -1)
(appt-activate 1)
(winner-mode 1)
(set-default 'truncate-lines t)
(set-default 'truncate-partial-width-windows t)
(set-default 'line-move-visual nil)

;; raise the limit for bindings and unwind-protect contexts
(setq max-specpdl-size 5000)

(setq server-use-tcp t)
(server-start)
(edit-server-start)

(when (file-exists-p (expand-file-name "~/.bash_profile"))
  (setq explicit-bash-args '("--login" "--init-file" "~/.bash_profile" "-i")))

(setq custom-file "~/.emacs.d/custom.el")
