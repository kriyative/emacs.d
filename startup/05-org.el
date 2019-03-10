(my-el-get-bundles
 org-mode
 org-gcal
 org-passwords)

;;;;;;;;;;;;;;;; org

(defvar org-journal-date-format "%Y-%m-%d %H:%M:%S"
  "Date format string for journal headings.")

(defun org-journal-entry ()
  "Create a new diary entry for today or append to an existing
one."
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
  (org-time-stamp-inactive
   (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun my-org-mode-hook ()
  (visual-line-mode)
  (org-display-inline-images))

(use-package org
  :config
  ;; fixme: built-in org is getting loaded somewhere, this forces
  ;; loading of el-get/org
  (setq features (remove 'org features))
  (require 'org)
  (unless (fboundp 'org-at-planning-p)
    (defun org-at-planning-p () nil))
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
	org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar"
        org-log-done 'time)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)
     (shell . t))))

(use-package org-agenda
  :config
  (setq org-agenda-include-diary t
        org-agenda-prefix-format (cons
                                  '(agenda . " %i %-16:c%?-12t% s")
                                  (remove-if (lambda (x)
                                               (eq (car x) 'agenda))
                                             org-agenda-prefix-format))))

(use-package org-passwords
  :config
  (setq org-passwords-file "~/.pwcrypt.gpg"
        org-passwords-random-words-dictionary "/etc/dictionaries-common/words"))

(use-package org-gcal
  :config
  ;; (setq org-gcal-client-id my-org-gcal-client-id
  ;;       org-gcal-client-secret my-org-gcal-client-secret
  ;;       org-gcal-file-alist my-org-gcal-file-alist)
  (setq org-agenda-mode-hook nil)
  ;; (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync)))
  ;; (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync)))
  )

(defvar org-gcal-multi-account-id nil)
(defvar org-gcal-multi-accounts-fun nil)
(defvar org-gcal-multi-accounts-cursor nil)

(defun org-gcal-multi-do-next ()
  (setq org-gcal-multi-accounts-cursor (cdr org-gcal-multi-accounts-cursor))
  (when org-gcal-multi-accounts-cursor
    (org-gcal-multi-do)))

(defun org-gcal-multi-do ()
  (let* ((i (car org-gcal-multi-accounts-cursor))
         (acct (cdr i)))
    (setq org-gcal-multi-account-id (car i)
          org-gcal-client-id (assoc-cdr 'org-gcal-client-id acct)
          org-gcal-client-secret (assoc-cdr 'org-gcal-client-secret acct)
          org-gcal-file-alist (assoc-cdr 'org-gcal-file-alist acct)
          org-gcal-token-file (assoc-cdr 'org-gcal-token-file acct)
          org-gcal-token-plist nil
          browse-url-browser-function 'browse-url-default-browser)
    (message "org-gcal-multi-do: %S..." org-gcal-multi-account-id)
    (funcall org-gcal-multi-accounts-fun)))

(defun org-gcal-multi-fetch ()
  (interactive)
  (setq org-gcal-multi-accounts-cursor org-gcal-accounts
        org-gcal-multi-accounts-fun
        (lambda ()
          (org-gcal-fetch nil 'org-gcal-multi-do-next)))
  (org-gcal-multi-do))

(defun org-gcal-multi-sync ()
  (interactive)
  (setq org-gcal-multi-accounts-cursor org-gcal-accounts
        org-gcal-multi-accounts-fun
        (lambda ()
          (org-gcal-sync nil nil nil 'org-gcal-multi-do-next)))
  (org-gcal-multi-do))

(defun org-gcal-token-fetch ()
  (org-gcal-refresh-token 'org-gcal-sync t)
  (message "%S: %S,%S"
           org-gcal-multi-account-id
           org-gcal-token-plist
           (org-gcal--get-access-token)))
