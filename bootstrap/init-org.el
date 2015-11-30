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

(defun org-mode-init ()
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (global-set-key (kbd "C-c j") 'org-journal-entry)
  (define-key org-mode-map "\C-c!" 'my-org-time-stamp-inactive)
  (define-key org-mode-map (kbd "C-c (") 'show-all)
  (define-key org-mode-map (kbd "C-c )") 'hide-sublevels)
  (define-key org-mode-map '[C-tab] nil)
  (define-key org-mode-map "\M-n" 'next-page)
  (define-key org-mode-map "\M-p" 'prev-page)
  (when org-agenda-files
    (add-hook 'display-time-hook 'show-daily-agenda))
  (setq org-export-html-postamble nil))

(eval-after-load 'org
  '(progn
     (require 'ob-clojure)
     (org-mode-init)))

(defun wicked/remember-review-file ()
  "Open `remember-data-file'."
  (interactive)
  (find-file-other-window remember-data-file))

(defun remember-init ()
  (global-set-key (kbd "C-c r") 'remember)
  (global-set-key (kbd "C-c R") 'wicked/remember-review-file))

(eval-after-load 'remember
  '(remember-init))

(defun setup-org-passwords ()
  (setq org-passwords-file "~/.pwcrypt.gpg"
        org-passwords-random-words-dictionary "/etc/dictionaries-common/words"))

(eval-after-load 'org-passwords
  '(setup-org-passwords))

(defun org-page-init ()
  (setq op/repository-directory "/home/ram/work/kriyative/kriyative-org-page/"
        op/site-domain "http://kriyative.github.io/"
        op/site-main-title "kriyative"
        op/site-sub-title "Musings of yet another Lisp programmer"
        op/personal-github-link "http://github.com/kriyative"
        op/personal-avatar nil ;; "http://www.gravatar.com/avatar/6dcf36ddd0aa73e04e125e3c10f6ff05.png"
        op/theme-root-directory "/home/ram/work/kriyative/mustache-themes/"
        op/theme 'kriyative
        op/doc-root "file:///home/ram/work/kriyative/kriyative-org-page"))

(eval-after-load 'org-page
  '(org-page-init))
