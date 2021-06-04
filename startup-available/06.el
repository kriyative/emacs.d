(rk-el-get-bundles
 hagleitn/speed-type
 ht
 iqbalansari/emacs-emojify
 (speed-type-patterns
  :url "https://gitlab.com/kriyative/speed-type-patterns.git"
  :features speed-type-patterns)
 key-chord)

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
  (add-hook 'mu4e-view-mode-hook 'emojify-mode))

;; (use-package key-chord)
;; (key-chord-define emacs-lisp-mode-map "df" "(defun  ()\n)\M-b\M-f\C-f")
;; (key-chord-define emacs-lisp-mode-map "up" "(use-package ")
;; (key-chord-define emacs-lisp-mode-map "((" "[")
;; (key-chord-define emacs-lisp-mode-map "ff" ffap)
;; (setq key-chord-two-keys-delay 0.08)

(when (file-exists-p diary-file)
  (diary 0))

(use-package server
  :config
  (setq server-socket-dir "~/.emacs.d/server/"
        server-name "server"
        server-use-tcp t)
  (server-force-delete)
  (server-start))

(when (fboundp 'mbsync-sync-accounts)
  (mbsync-sync-accounts))

(defun rk-aws-doc (commands)
  (interactive "sCommand: ")
  (let ((bufname (concat "*AWS-doc: " commands "*")))
    (pop-to-buffer
     (or (get-buffer bufname)
         (let ((buf (get-buffer-create bufname))
               (args (append
                      '("--color" "off"
                        "--no-cli-pager"
                        "--no-paginate")
                      (split-string commands)
                      '("help"))))
           (message "rk-aws-doc: args=%S" args)
           (with-current-buffer buf
             (apply 'call-process
                    "aws"
                    nil
                    t
                    t
                    args)
             (goto-char (point-min))
             (while (re-search-forward "." nil t)
               (replace-match "")))
           buf)))))

