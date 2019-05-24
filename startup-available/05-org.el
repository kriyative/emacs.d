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
  (force-require 'org)
  (force-require 'org-compat)
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
  (force-require 'org-macs)
  ;; (setq org-gcal-client-id my-org-gcal-client-id
  ;;       org-gcal-client-secret my-org-gcal-client-secret
  ;;       org-gcal-file-alist my-org-gcal-file-alist)
  (setq org-agenda-mode-hook nil
	org-gcal-auto-archive nil)
  ;; (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync)))
  ;; (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync)))
  )

(defvar org-gcal-multi-account-id nil)
(defvar org-gcal-multi-accounts-fun nil)
(defvar org-gcal-multi-accounts-cursor nil)

(defun org-gcal-multi-do-acct (spec)
  (let* ((acct (cdr spec)))
    (setq org-gcal-multi-account-id (car spec)
          org-gcal-client-id (assoc-cdr 'org-gcal-client-id acct)
          org-gcal-client-secret (assoc-cdr 'org-gcal-client-secret acct)
          org-gcal-file-alist (assoc-cdr 'org-gcal-file-alist acct)
          org-gcal-token-file (assoc-cdr 'org-gcal-token-file acct)
          org-gcal-token-plist nil
          browse-url-browser-function 'browse-url-default-browser)
    (message "org-gcal-multi-do: %S..." org-gcal-multi-account-id)
    (org-gcal-fetch)))

(defun org-gcal-multi-fetch-loop ()
  (deferred:$
    (deferred:next
      (lambda ()
	(message "org-gcal-multi-fetch starting ...")
	(org-gcal-multi-do-acct (first org-gcal-accounts))))
    (deferred:nextc it
      (lambda (accounts)
	(message "here: %S" accounts)
	(when accounts
	  ;; (org-gcal-multi-do-acct (first org-gcal-accounts))
	  (deferred:nextc (deferred:wait 0) self)
	  (cdr accounts))))
    (deferred:nextc it
      (lambda (x)
	(message "org-gcal-multi-fetch done")))))

(defun org-gcal-multi-fetch ()
  (interactive)
  (deferred:$
    (deferred:next
      (lambda ()
	(org-gcal-multi-do-acct (first org-gcal-accounts))))
    (deferred:nextc it
      (lambda ()
	(org-gcal-multi-do-acct (second org-gcal-accounts))))))

;; (org-gcal-multi-fetch)

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
