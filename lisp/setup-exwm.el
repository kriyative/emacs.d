(el-get-bundle ch11ng/xelb)
(el-get-bundle ch11ng/exwm)

(defun exec! (command)
  (interactive (list (read-shell-command "$ ")))
  (let* ((program-and-args (if (stringp command)
                               (split-string command)
                             command))
         (program (car program-and-args))
         (args (cdr program-and-args))
         (console (get-buffer-create "*Console*"))
         (pt (with-current-buffer console
               (goto-char (point-max))
               (insert "$ "
                       (mapconcat 'identity program-and-args " ")
                       "\n")
               (point)))
         (ret (apply 'call-process program nil console t args))
         (out (with-current-buffer console
                (buffer-substring-no-properties pt (point)))))
    (if (and (numberp ret) (= 0 ret))
        out
      (throw 'exec!-error (list ret out)))))

(defun spawn& (command)
  (interactive (list (read-shell-command "$ ")))
  (let* ((program-and-args (if (stringp command)
                               (split-string command)
                             command))
         (program (car program-and-args))
         (program-name (file-name-nondirectory program))
         (args (cdr program-and-args)))
    (apply 'start-process program-name program-name program args)))

(global-set-key (kbd "s-!") 'exec!)
(global-set-key (kbd "s-&") 'spawn&)

(use-package mu4e-alert
  :config
  (progn
    (mu4e-alert-set-default-style 'log)
    ;; (mu4e-alert-set-default-style 'notifications)
    ;; (alert-add-rule :category "mu4e-alert" :style 'fringe)
    ;; (alert-add-rule :category "mu4e-alert" :style 'mode-line)
    (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
    (add-hook 'mu4e-message-changed-hook 'mu4e-alert-update-mail-count-modeline)))

(defun my-exwm-init ()
  (exwm-input-set-simulation-keys
   '(([?\C-b] . left)
     ([?\C-f] . right)
     ([?\C-p] . up)
     ([?\C-n] . down)
     ([?\C-a] . home)
     ([?\M-<] . home)
     ([?\C-e] . end)
     ([?\M->] . end)
     ([?\M-v] . prior)
     ([?\C-v] . next)
     ([?\C-d] . delete)
     ([?\C-k] . (S-end delete))
     ([?\M-w] . ?\C-c)
     ([?\C-w] . ?\C-x)
     ([?\C-y] . ?\C-v)
     ([?\C-\M-b] . (M-left))
     ([?\C-\M-f] . (M-right))
     ([?\C-s] . ?\C-f)
     ([?\C-\M-s] . ?\/)
     ))
  (spawn& "/usr/lib/gnome-settings-daemon/gsd-xsettings")
  (exec! (concat "synclient"
                 " VertTwoFingerScroll=1"
                 " VertScrollDelta=-111"
                 " HorizScrollDelta=-111"
                 " HorizTwoFingerScroll=1"
                 " TapButton1=1"
                 " TapButton2=3"
                 " TapButton3=2"
                 " PalmDetect=1"))
  (exec! "setxkbmap -option ctrl:nocaps -option ctrl:ralt_rctrl")
  (exec! "xsetroot -cursor_name left_ptr -bg black")
  (spawn& "dropbox start")
  (start-redshift)
  (start-screensaver)
  (emms-mode-line -1)
  (mu4e-alert-enable-mode-line-display)
  (setq browse-url-firefox-arguments '("-new-window")))

(defun lock-screen ()
  (interactive)
  (start-screensaver)
  (sleep-for 1)
  (exec! "xscreensaver-command -lock"))

(defvar *screensaver-proc* nil)

(defun stop-screensaver ()
  (interactive)
  (exec! "xset -dpms")
  (kill-process (or *screensaver-proc* "xscreensaver")))

(defun start-screensaver ()
  (interactive)
  (unless (and *screensaver-proc*
               (process-live-p *screensaver-proc*))
    (exec! "xset +dpms")
    (setq *screensaver-proc* (spawn& "xscreensaver -no-splash"))))

(defun start-redshift ()
  (interactive)
  (let ((redshift-proc (get-process "redshift")))
    (unless (and redshift-proc
                 (process-live-p redshift-proc))
      (spawn& "redshift"))))

(defun stop-redshift ()
  (interactive)
  (let ((redshift-proc (get-process "redshift")))
    (when (and redshift-proc (process-live-p redshift-proc))
      (kill-process redshift-proc)
      (exec! "redshift -x"))))

(defun setup-exwm-bind-keys ()
  (global-set-key (kbd "s-l") 'lock-screen)
  (define-key exwm-mode-map "\C-c\C-l" 'lock-screen)
  (global-set-key "\C-c\C-l" 'lock-screen)
  (define-key exwm-mode-map "\C-c\C-k" nil)
  (define-key exwm-mode-map "\C-c!" 'exec!)
  (global-set-key "\C-c!" 'exec!)
  (define-key exwm-mode-map "\C-c&" 'spawn&)
  (global-set-key "\C-c&" 'spawn&)
  (dolist (k '(XF86AudioLowerVolume
               XF86AudioRaiseVolume
               XF86AudioPlay
               XF86AudioStop
               XF86AudioPrev
               XF86AudioNext
               XF86MonBrightnessUp
               XF86MonBrightnessDown
               print
               ?\C-\;))
    (pushnew k exwm-input-prefix-keys)))

(use-package exwm
  :config
  (setq display-time-day-and-date t)
  (display-time)
  (setup-exwm-bind-keys))

(use-package exwm-config
  :config
  (setq exwm-workspace-number 4)
  ;; (add-hook 'exwm-floating-setup-hook #'exwm-layout-hide-mode-line)
  ;; (add-hook 'exwm-floating-exit-hook #'exwm-layout-show-mode-line)
  (exwm-config-default)
  (my-exwm-init))

;; (use-package exwm-cm
;;   :config
;;   (exwm-cm-enable))

(use-package exwm-randr
  :config (exwm-randr-enable))

(el-get-bundle kriyative/highlight-focus)
(use-package highlight-focus
  :config
  (set-face-background 'mode-line "#444")
  (set-face-foreground 'mode-line "#eee")
  (setq highlight-focus:face 'mode-line
        highlight-focus:face-property :background
        highlight-focus:face-property-value "darkgreen"))

;; (el-get-bundle Bad-ptr/common-header-mode-line.el)
;; (use-package common-header-line-mode.el
;;   :config
;;   (common-mode-line-mode 1))

(global-set-key (kbd "<XF86AudioPlay>") 'emms-pause)
(global-set-key (kbd "<XF86AudioStop>") 'emms-stop)
(global-set-key (kbd "<XF86AudioPrev>") 'emms-previous)
(global-set-key (kbd "<XF86AudioNext>") 'emms-next)

(el-get-bundle flexibeast/pulseaudio-control)
(use-package pulseaudio-control)

(global-set-key (kbd "<XF86AudioLowerVolume>") 'pulseaudio-control-decrease-volume)
(global-set-key (kbd "<XF86AudioRaiseVolume>") 'pulseaudio-control-increase-volume)
(pulseaudio-control-default-keybindings)

(define-key exwm-mode-map (kbd "s-q") #'exwm-input-send-next-key)
(define-key exwm-mode-map (kbd "s-S-q") #'exwm-input-send-next-key)

(el-get-bundle browse-kill-ring)

(defun browse-kill-ring-select (&optional quit)
  (interactive "P")
  (let* ((buf (current-buffer))
         (pt (point))
         (str (browse-kill-ring-current-string buf pt)))
    (setq kill-ring-yank-pointer
          (browse-kill-ring-current-kill-ring-yank-pointer buf pt))
    (gui-set-selection nil str)
    (gui-set-selection 'CLIPBOARD str)
    (browse-kill-ring-quit)))

(use-package browse-kill-ring
  :bind (("\C-x\C-y" . browse-kill-ring)
         :map browse-kill-ring-mode-map
         ("<return>" . browse-kill-ring-select))
  :config
  (setq browse-kill-ring-show-preview nil
        browse-kill-ring-separator "----------------"
        select-enable-primary t
        yank-pop-change-selection t))

(defun banish-mouse ()
  (interactive)
  (mouse-avoidance-banish-mouse))

(global-set-key (kbd "\C-c\C-b") 'banish-mouse)
(global-set-key (kbd "s-C-b") 'banish-mouse)

(use-package battery
  :config
  (setq battery-mode-line-format " %L:%b%p%%"
        battery-update-interval 10)
  (display-battery-mode))

(use-package wifi
  :load-path "~/.emacs.d/packages"
  :config
  (setq wifi-mode-line-format " %E:%S"
        wifi-update-interval 10)
  (display-wifi-mode))

(defun change-brightness (amount)
  (exec! (concat "xbacklight "
                 (if (< 0 amount) "-inc" "-dec")
                 " "
                 (prin1-to-string (abs amount))))
  (let ((v (with-temp-buffer
             (call-process "xbacklight" nil (current-buffer) nil "-get")
             (goto-char (point-min))
             (read (current-buffer)))))
    (message "brightness: %S%%" (round v))))

(defun decrease-brightness ()
  (interactive)
  (change-brightness -1))

(defun increase-brightness ()
  (interactive)
  (change-brightness 1))

(global-set-key (kbd "<XF86MonBrightnessUp>") 'increase-brightness)
(global-set-key (kbd "<XF86MonBrightnessDown>") 'decrease-brightness)

(defun scrot (&optional n)
  (interactive "p")
  (let ((cmd `("scrot"
               "-e"
               "echo $f"
               ,@(when (< 1 n)
                   (list "-d" (prin1-to-string n))))))
    
    (message "Screenshot saved in %s" (exec! cmd))))

(defun scrot-select ()
  (interactive)
  (message "Select area to capture in screenshot")
  (message "Screenshot saved in %s"
           (exec! '("scrot" "-s" "-e" "echo $f"))))

(global-set-key (kbd "<print>") 'scrot)
(global-set-key (kbd "C-<print>") 'scrot-select)

(defun pulseaudio-set-default-sink (sink)
  (pulseaudio-control--call-pactl (concat "set-default-sink " sink))
  (setq pulseaudio-control--current-sink sink))

(defun set-audio-profile (profile)
  (interactive
   (list
    (completing-read "Profile: "
                     '("mpow.a2dp"
                       "mpow.headset"
                       "built-in"))))
  (exec! (list "set-audio-profile" profile))
  (let ((sink (cdr (car (pulseaudio-control--get-sinks)))))
    (pulseaudio-set-default-sink sink)))

(defun systemd-suspend ()
  (interactive)
  (exec! "systemctl suspend"))

(defun exwm-manage-finish-hook ()
  ;; (message "exwm-manage-finish-hook: %S" exwm-class-name)
  ;; (when (member exwm-class-name '("URxvt"))
  ;;   (exwm-input-release-keyboard))
  )

(add-hook 'exwm-manage-finish-hook 'exwm-manage-finish-hook)

(defun exwm-update-title-hook ()
  (when (and exwm-class-name (string-match "Firefox" exwm-class-name))
    (exwm-workspace-rename-buffer exwm-title)))

(add-hook 'exwm-update-title-hook 'exwm-update-title-hook)

;; (require 'exwm-randr)
;; (setq exwm-randr-workspace-output-plist '(0 "eDP1" 1 "DP2-2" 2 "HDMI1"))
;; (defun exwm-randr-screen-change-hook ()
;;   (start-process-shell-command
;;    "xrandr"
;;    nil
;;    "xrandr --output eDP1 --left-of HDMI1 --auto"))
;; (add-hook 'exwm-randr-screen-change-hook 'exwm-randr-screen-change-hook)
;; (exwm-randr-enable)

(setq exwm-workspace-show-all-buffers t)

(defun exwm-workspace-pull-window (buffer-or-name)
  (interactive
   (let ((collection (mapcar (lambda (x)
                               (let ((w (cdr x)))
                                 (cons (buffer-name w) w)))
                             exwm--id-buffer-alist)))
     (list
      (assoc-cdr (completing-read "Select window: " collection) collection))))
  (let* ((buffer (get-buffer buffer-or-name))
         (id (exwm--buffer->id buffer)))
    (unless id
      (error "Not a window: %S" buffer))
    (exwm-workspace-move-window exwm-workspace-current-index id)))
