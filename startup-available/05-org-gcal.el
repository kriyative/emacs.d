(rk-el-get-bundles org-gcal)

(use-package org-gcal
  :after org
  :config
  (setq org-agenda-mode-hook nil
        org-gcal-auto-archive nil))

(defvar org-gcal-multi-account-id nil)
(defvar org-gcal-multi-accounts-fun nil)
(defvar org-gcal-multi-accounts-cursor nil)

(defun org-gcal-multi-do-acct (spec)
  (cl-labels
      ((assoc-cdr (x alist) (cdr (assoc x alist))))
    (let* ((acct (cdr spec)))
      (setq org-gcal-multi-account-id (car spec)
            org-gcal-client-id (assoc-cdr 'org-gcal-client-id acct)
            org-gcal-client-secret (assoc-cdr 'org-gcal-client-secret acct)
            org-gcal-file-alist (assoc-cdr 'org-gcal-file-alist acct)
            org-gcal-token-file (assoc-cdr 'org-gcal-token-file acct)
            org-gcal-token-plist nil)
      (message "org-gcal-multi-do: %S..." org-gcal-multi-account-id)
      (org-gcal-fetch))))

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
