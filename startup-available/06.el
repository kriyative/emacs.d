(defun my-midnight-hook ()
  (org-gcal-multi-fetch)
  (org-agenda-list nil nil 'day))

(use-package midnight
  :config
  (midnight-delay-set 'midnight-delay "9:00am")
  (add-hook 'midnight-hook 'my-midnight-hook))

(when (file-exists-p diary-file)
  (diary 0))
(setq server-use-tcp t)
(server-start)
