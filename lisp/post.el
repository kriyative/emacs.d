;; things to run at the end of emacs initialization

(when org-agenda-files
  (add-hook 'display-time-hook 'show-daily-agenda))


