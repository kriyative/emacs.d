;; ss -- system stats

(require 'mls-common)
(require 'mls-battery)
(require 'mls-cpu)
(require 'mls-memory)
(require 'mls-disk)
(require 'mls-misc)

(mls-battery-start)
(mls-cpu-start)
(mls-memory-start)
(mls-disk-start)
(mls-misc-start)

(defun ss/format-battery-stats (fmt)
  (let ((stats (mls-battery-fetch)))
    (mls-format-expand mls-battery-formatters
                       (if (stringp fmt)
                           fmt
                         (funcall fmt stats))
                       stats)))

;; (ss/format-battery-stats "BAT[%p% %t %B]")

(defun ss/format-cpu-stats (fmt)
  (let ((stats (mls-cpu-fetch)))
    (mls-format-expand mls-cpu-formatters fmt stats)))

;; (ss/format-cpu-stats "%A%")

(defun ss/format-memory-stats (fmt)
  (let ((stats (mls-memory-fetch)))
    (mls-format-expand mls-memory-formatters fmt stats)))

;; (ss/format-memory-stats "%R%")

(defun ss/format-disk-stats (fmt)
  (let ((stats (mls-disk-fetch)))
    (mls-format-expand mls-disk-formatters fmt stats)))

(defun mls-misc-fetch ()
  (let* ((load (map 'list
                    (lambda (x) (/ x 100.0))
                    (load-average)))
         (boot-time (string-to-number
                     (shell-command-to-string
                      "cat /proc/stat | awk '{if(NR == 6) print $2}'")))
         (emacs-uptime (emacs-uptime mls-misc-emacs-uptime-format))
         (system-uptime (format-seconds mls-misc-system-uptime-format
                                        (- (float-time (current-time))
                                           boot-time))))
    (list load boot-time emacs-uptime system-uptime)))

(defun ss/format-misc-stats (fmt)
  (let ((stats (mls-misc-fetch)))
    (mls-format-expand mls-misc-formatters fmt stats)))

(defun dbm-to-signal (dbm)
  (when dbm
    (let* ((dbm-floor 20.0)
           (dbm-range (- 90.0 dbm-floor)))
      (min 1 (- 1 (/ (- (abs dbm) dbm-floor) dbm-range))))))

(defun ss/format-wifi-stats ()
  (with-temp-buffer
    (call-process "iwconfig" nil t nil "wlan0")
    (goto-char (point-min))
    (let* ((essid (and (re-search-forward "ESSID:\"\\([^\"]*\\)\"" nil t)
                       (match-string 1)))
           (dbm (and (re-search-forward "Signal level=\\([-0-9]*\\) dBm" nil t)
                     (string-to-number (match-string 1)))))
      (if dbm
          (format "NET[%s %ddBm %d%%]" essid dbm (* 100 (dbm-to-signal dbm)))
        "NET[N/A]"))))

;; (ss/format-wifi-stats)

(defun ss/current ()
  (mapconcat 'identity
             (list
              (concat (user-login-name) "@" (system-name))
              (ss/format-battery-stats
               (lambda (stats)
                 (if (equal "AC" (car (nth 4 stats)))
                     "BAT[%p% AC]"
                   "BAT[%p% %t %B]")))
              (ss/format-cpu-stats "CPU[%C0% %C1% %C2% %C3%]")
              (ss/format-memory-stats "MEM[%R% %fMB]")
              (ss/format-disk-stats "DISK[%p%]")
              (ss/format-misc-stats "LOAD[%L1]")
              (ss/format-wifi-stats)
              (current-time-string))
             " "))

;; (ss/current)

(defun ss/info ()
  (interactive)
  (message "%s" (ss/current)))

(provide 'ss)
