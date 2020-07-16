(rk-el-get-bundles
 hagleitn/speed-type
 ht
 iqbalansari/emacs-emojify)

(defun rk--file-age (file)
  (float-time
   (time-subtract (current-time)
                  (file-attribute-modification-time
                   (file-attributes file)))))

(defun rk--org-gcal-files ()
  (reduce (lambda (all-files account)
            (cl-concatenate 'list
                            all-files
                            (mapcar 'cdr
                                    (cdr
                                     (assoc 'org-gcal-file-alist
                                            (cdr account))))))
          org-gcal-accounts
          :initial-value nil))

(defun rk--midnight-hook ()
  (when (fboundp 'org-gcal-multi-fetch)
    (let ((gcal-file (cdadr
                      (assoc 'org-gcal-file-alist
                             (cdr (first org-gcal-accounts))))))
      (when (< 3600 (rk--file-age gcal-file))
        (org-gcal-multi-fetch))
      (org-agenda-list nil nil 'day))))

(use-package midnight
  :config
  (midnight-delay-set 'midnight-delay "9:00am")
  (add-hook 'midnight-hook 'rk--midnight-hook))

(use-package ht)

(use-package emojify
  :config
  (global-emojify-mode 1))

(when (file-exists-p diary-file)
  (diary 0))

(setq server-socket-dir "~/.emacs.d/server/"
      server-use-tcp t)
(server-start)

(when (fboundp 'run)
  (run "mbsync-all -s" :hide-buffer t))
