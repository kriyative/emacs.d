(defun fortune-computers ()
  (interactive)
  (fortune (concat fortune-dir "/computers")))

(use-package fortune
  :bind (:map user-commands-prefix-map
              ("ff" . fortune)
              ("fc" . fortune-computers))
  :config
  (setq fortune-dir "/usr/share/games/fortunes"
        fortune-file "/usr/share/games/fortunes/fortunes"))

(defmacro with-elscreen (screen &rest body)
  (declare (indent 1))
  `(if (fboundp 'elscreen-goto)
       (save-excursion
         (let ((current-screen (when (fboundp 'elscreen-get-current-screen)
                                 (elscreen-get-current-screen))))
           (when (fboundp 'elscreen-goto)
             (elscreen-goto ,screen))
           (progn ,@body)
           (when current-screen
             (elscreen-goto current-screen))))
     (progn
       ,@body)))

(defun rk--goto-daily-agenda ()
  (interactive)
  (save-excursion
    (with-elscreen 0
      (org-agenda-list))))

(defun rk--midnight-hook ()
  ;; (when (fboundp 'rk-org-gcal-multi-fetch-if-stale)
  ;;   (rk-org-gcal-multi-fetch-if-stale))
  ;; (org-caldav-sync)
  (rk--goto-daily-agenda))

(use-package midnight
  :config
  (midnight-delay-set 'midnight-delay "9:00am")
  (add-hook 'midnight-hook 'rk--midnight-hook))

(when (file-exists-p diary-file)
  (diary 0))

(defun rk--emacs-server-running-p ()
  ;; has to be async call, if current emacs is the server
  (let ((proc (make-process
               :name "emacsclient-check"
               :command '("emacsclient" "-e" "emacs-version"))))
    ;; fixme: add a timeout for the poll
    (while (not (eq 'exit (process-status proc)))
      (sleep-for 0 100))
    (eq 0 (process-exit-status proc))))

(let ((socket-dir "~/.emacs.d/server/")
      (socket-file "server"))
  (use-package server
    ;; there can be only one!
    :if (not (rk--emacs-server-running-p))
    :config
    (setq server-socket-dir socket-dir
          server-name socket-file
          server-use-tcp t)
    (server-force-delete)
    (server-start)))

(provide 'init-misc)
