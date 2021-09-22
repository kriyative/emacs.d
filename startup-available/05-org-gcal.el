(rk-el-get-bundles persist
                   org-gcal)

(use-package org-gcal
  :after org
  :config
  (setq org-agenda-mode-hook nil
        org-gcal-auto-archive nil
        org-gcal-client-id nil
        org-gcal-client-secret nil
        org-gcal-file-alist nil
        org-gcal-token-file nil))

(defun rk--org-gcal-multi-fetch (accounts)
  (dolist (account accounts)
    (destructuring-bind (&key name
                              client-id
                              client-secret
                              file-alist
                              token-file)
        account
      (let ((buf (get-buffer-create (concat "org-gcal: " name))))
        (with-current-buffer buf
          (make-local-variable 'org-gcal-client-id)
          (make-local-variable 'org-gcal-client-secret)
          (make-local-variable 'org-gcal-file-alist)
          (make-local-variable 'org-gcal-token-file)
          (setq org-gcal-client-id client-id
                org-gcal-client-secret client-secret
                org-gcal-file-alist file-alist
                org-gcal-token-file token-file)
          (org-gcal-fetch))))))

(defvar rk--org-gcal-accounts nil)

(defun rk-org-gcal-multi-fetch ()
  (interactive)
  (let ((browse-url-browser-function 'browse-url-chromium))
   (rk--org-gcal-multi-fetch rk--org-gcal-accounts)))

(defun rk--org-gcal-files ()
  (reduce (lambda (all-files account)
            (cl-concatenate 'list
                            all-files
                            (mapcar 'cdr
                                    (getf account :file-alist))))
          rk--org-gcal-accounts
          :initial-value nil))

(defun rk--org-gcal-stalep ()
  (let ((gcal-file (first (rk--org-gcal-files))))
    (< 3600 (rk--file-age gcal-file))))

(defun rk-org-gcal-multi-fetch-if-stale ()
  (when (rk--org-gcal-stalep)
    (rk-org-gcal-multi-fetch))
  (org-agenda-list nil nil 'day))
