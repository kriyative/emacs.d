;;;;;;;;;;;;;;;; dependencies ;;;;;;;;;;;;;;;;

(rk-require-packages
 dictionary
 edit-server
 magit-popup)

;;;;;;;;;;;;;;;; commands ;;;;;;;;;;;;;;;;

(defun rk-set-mode-line-color (color)
  (interactive (list (read-color "Mode-line color: ")))
  (set-face-background 'mode-line color))

(defun rk-jitsi (&optional url)
  (interactive "sURL: ")
  (browse-url (concat url "#config.startWithVideoMuted=true")))

;;;;;;;;;;;;;;;; packages ;;;;;;;;;;;;;;;;

(use-package dictionary
  :bind
  (("\C-cs" . dictionary-search)
   ("\C-cm" . dictionary-match-words)))

(defun iso-calendar ()
  (interactive)
  (setq european-calendar-style nil)
  (setq calendar-date-display-form
        '(year
          "-"
          (if (< (length month) 2) (concat "0" month) month)
          "-"
          (if (< (length day) 2) (concat "0" day) day)))
  (setq diary-date-forms
        '((year "-" month "-" day "[^0-9]")
          (month "/" day "[^/0-9]")
          (month "/" day "/" year "[^0-9]")
          (monthname " *" day "[^,0-9]")
          (monthname " *" day ", *" year "[^0-9]")
          (dayname "\\W")))
  (cond
   ((string-match "^2[12]" emacs-version)
    (update-calendar-mode-line))
   (t
    (when (fboundp 'calendar-update-mode-line)
      (calendar-update-mode-line)))))

(use-package calendar
  :config
  (iso-calendar)
  (add-hook 'diary-display-hook 'fancy-diary-display)
  ;; (add-hook 'calendar-load-hook 'mark-diary-entries)
  (add-hook 'list-diary-entries-hook 'sort-diary-entries t)
  (setq display-time-day-and-date nil
        display-time-world-list '(("Pacific/Honolulu" "Honolulu")
                                  ("America/Anchorage" "Anchorage")
                                  ("America/Los_Angeles" "Los Angeles")
                                  ("America/Phoenix" "Phoenix")
                                  ("America/Chicago" "Chicago")
                                  ("America/New_York" "New York")
                                  ("Europe/London" "London")
                                  ("Europe/Paris" "Paris")
                                  ("Asia/Calcutta" "Calcutta")
                                  ("Asia/Singapore" "Singapore")
                                  ("Australia/Sydney" "Sydney")
                                  ("Pacific/Auckland" "Auckland"))
        display-time-world-time-format "%a %d %b %R %Z"))

(use-package edit-server
  :config
  (setq edit-server-default-major-mode 'normal-mode
        edit-server-new-frame nil)
  (edit-server-start))

(use-package magit-popup)

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

(defun rk-countdown-timer-alert (message)
  (let ((alert-default-style 'mode-line))
    (alert message)))

(defun rk-countdown-timer (&optional seconds message)
  (interactive
   (list
    (if current-prefix-arg
        (read-number "Seconds: " 60)
      60)
    (if current-prefix-arg
        (read-string "Message: ")
      "Countdown timer alert!")))
  (run-with-timer seconds nil 'rk-countdown-timer-alert message))

(defun rk-make-window-east ()
  (interactive)
  (split-window (frame-root-window) nil 'right))

(defun rk-make-window-south ()
  (interactive)
  (split-window (frame-root-window) nil 'below))

(rk-bind-keys
 '(("RET" rk-countdown-timer)
   ("C-x 2" rk-make-window-south)
   ("C-x 3" rk-make-window-east))
 user-commands-prefix-map)

;;;;;;;;;;;;;;;; startup ;;;;;;;;;;;;;;;;

(defun gnome-screenshot (args)
  (interactive (list (gnome-screenshot-arguments)))
  (exec! (list* "gnome-screenshot" args)))

(magit-define-popup gnome-screenshot-popup
  "Show popup buffer featuring Gnome screenshot commands"
  'exwm-commands
  :switches '((?a "Area"      "--area")
              (?b "Border"    "--include-border")
              (?c "Clipboard" "--clipboard")
              (?w "Window"    "--window"))
  :options  '((?d "Delay" "--delay=" read-number)
              (?e "Border effect" "--border-effect="
                  (lambda (prompt &rest args)
                    (completing-read
                     prompt
                     '("shadow" "border" "vintage" "none")))))
  :actions  '((?x "Execute" gnome-screenshot))
  :default-action 'gnome-screenshot)

(defun scrot-screenshot (args)
  (interactive (list (scrot-screenshot-arguments)))
  (exec! (list* "scrot" (append args '("-e" "mv $f ~/Pictures/")))))

(magit-define-popup scrot-screenshot-popup
  "Show popup menu to invoke `scrot` screenshot options"
  'exwm-commands
  :switches '((?b "Include Border" "--border")
              (?c "Show Count"     "--count")
              (?s "Select Window"  "--select")
              (?z "Silent"         "--silent"))
  :options  '((?t "Create Thumbnail" "--thumb")
              (?d "Delay"            "--delay=" read-number)
              (?q "Quality"          "--quality=" read-number))
  :actions  '((?x "Execute" scrot-screenshot))
  :default-action 'scrot-screenshot)

(global-set-key (kbd "<print>") 'scrot-screenshot-popup)
