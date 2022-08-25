(rk-el-get-bundles org-caldav)

(use-package org-caldav)

(defun rk--midnight-hook ()
  ;; (when (fboundp 'rk-org-gcal-multi-fetch-if-stale)
  ;;   (rk-org-gcal-multi-fetch-if-stale))
  ;; (org-caldav-sync)
  )

(use-package midnight
  :config
  (midnight-delay-set 'midnight-delay "9:00am")
  (add-hook 'midnight-hook 'rk--midnight-hook))

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
