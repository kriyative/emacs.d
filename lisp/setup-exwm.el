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
         (program-buffer (concat " *" program-name))
         (args (cdr program-and-args)))
    (apply 'start-process program-name program-buffer program args)))

(use-package mu4e-alert
  :config
  (progn
    (mu4e-alert-set-default-style 'log)
    ;; (mu4e-alert-set-default-style 'notifications)
    ;; (alert-add-rule :category "mu4e-alert" :style 'fringe)
    ;; (alert-add-rule :category "mu4e-alert" :style 'mode-line)
    (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
    (add-hook 'mu4e-message-changed-hook 'mu4e-alert-update-mail-count-modeline)))

(defun my-exwm-input-set-simulation-keys (keys)
  (dolist (k keys)
    (exwm-input-set-simulation-key (car k) (cdr k))))

(defun my-exwm-init ()
  (my-exwm-input-set-simulation-keys
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
     ([?\C-k] . (S-end C-x))
     ([?\M-w] . ?\C-c)
     ([?\C-w] . ?\C-x)
     ([?\C-y] . ?\C-v)
     ([?\C-\M-b] . (M-left))
     ([?\C-\M-f] . (M-right))
     ([?\C-s] . ?\C-f)
     ([?\C-\M-s] . ?\/)))
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
  (exec! (concat "setxkbmap "
                 "-option ctrl:nocaps"
                 ;; "-option ctrl:ralt_rctrl"
                 ))
  (exec! "xsetroot -cursor_name left_ptr -bg black")
  (spawn& "dropbox start")
  (start-redshift)
  (start-screensaver)
  (emms-mode-line -1)
  (mu4e-alert-enable-mode-line-display)
  (setq browse-url-firefox-arguments '("-new-window")
        exwm-workspace-show-all-buffers nil))

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

(defmacro setup-exwm-input-set-keys (&rest key-bindings)
  `(dolist (kb ',key-bindings)
     (destructuring-bind (key cmd)
         kb
       (exwm-input-set-key (kbd key) cmd))))

(defun dired-home-dir ()
  (interactive)
  (dired (expand-file-name "~/")))

(defun setup-exwm-bind-keys ()
  (define-key exwm-mode-map "\C-c\C-k" nil)
  (define-key exwm-mode-map "\C-c!" 'exec!)
  (setup-exwm-input-set-keys
   ("s-!" exec!)
   ("\C-c!" exec!)
   ("s-&" spawn&)
   ("\C-c&" spawn&)
   ("\C-xb" exwm-workspace-switch-to-buffer)
   ("<XF86Explorer>" dired-home-dir))
  (dolist (k '(XF86AudioLowerVolume
               XF86AudioMute
               XF86AudioRaiseVolume
               XF86AudioPlay
               XF86AudioStop
               XF86AudioPrev
               XF86AudioNext
               XF86MonBrightnessUp
               XF86MonBrightnessDown
               XF86Explorer
               pause
               s-pause
               print
               \C-print
               ?\C-\;
               f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12))
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
  (ido-mode -1)
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

(setup-exwm-input-set-keys
 ("<XF86AudioPlay>" emms-pause)
 ("<XF86AudioStop>" emms-stop)
 ("<XF86AudioPrev>" emms-previous)
 ("<XF86AudioNext>" emms-next))

(el-get-bundle flexibeast/pulseaudio-control)
(use-package pulseaudio-control)

(setup-exwm-input-set-keys
 ("<XF86AudioLowerVolume>" pulseaudio-control-decrease-volume)
 ("<XF86AudioRaiseVolume>" pulseaudio-control-increase-volume)
 ("<XF86AudioMute>" pulseaudio-control-toggle-current-sink-mute))
(pulseaudio-control-default-keybindings)

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
         ("<return>" . browse-kill-ring-insert-and-quit))
  :config
  (setq browse-kill-ring-show-preview nil
        browse-kill-ring-separator "----------------"
        browse-kill-ring-display-style 'one-line
        select-enable-primary t
        yank-pop-change-selection t))

(defun banish-mouse ()
  (interactive)
  (mouse-avoidance-banish-mouse))

(setup-exwm-input-set-keys
 ("\C-c\C-b" banish-mouse)
 ("s-C-b" banish-mouse))

(use-package battery
  :config
  (setq battery-mode-line-format " %L:%b%p%% "
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

(defun decrease-brightness (&optional arg)
  (interactive "p")
  (change-brightness (* -1 (or arg 1))))

(defun increase-brightness (&optional arg)
  (interactive "p")
  (change-brightness (* 1 (or arg 1))))

(setup-exwm-input-set-keys
 ("s-<pause>" lock-screen)
 ("C-<escape>" lock-screen)
 ("<XF86MonBrightnessUp>" increase-brightness)
 ("<XF86MonBrightnessDown>" decrease-brightness))

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

(defun pulseaudio-set-default-sink (sink)
  (pulseaudio-control--call-pactl (concat "set-default-sink " sink))
  (setq pulseaudio-control--current-sink sink))

(defun set-audio-profile (profile)
  (interactive
   (list
    (completing-read "Profile: "
                     '("a2dp"
                       "built-in"
                       "dp"
                       "headset"
                       "usb"))))
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

;;; xrandr info in frame-parameters
;; [[mu4e:msgid:ch11ng/exwm/issues/379/374007853@github.com]]
;; (mapcar (lambda (i)
;;           (vector (frame-parameter i 'exwm-randr-output)
;;                   (frame-parameter i 'exwm-active)))
;;         exwm-workspace--list)

(el-get-bundle gpastel
  :url "https://gitlab.petton.fr/DamienCassou/gpastel.git"
  :features gpastel)

(defun exwm-send-paste-key ()
  (interactive)
  (exwm-input--fake-key 22))

(define-advice browse-kill-ring-insert-and-highlight (:around (old-function str) exwm-paste)
  "Paste the selection appropriately in exwm mode buffers"
  (if (derived-mode-p 'exwm-mode)
      (progn
        (kill-new str)
        (call-interactively 'exwm-send-paste-key))
    (funcall old-function str)))

(use-package gpastel
  :config
  ;; fixme: reexec-daemon doesn't start correctly
  ;; (gpastel-start-listening)
  (spawn& (list gpastel-gpaste-client-command "start"))
  (setq interprogram-paste-function (lambda ())
        ;; No need to save the system clipboard before killing in
        ;; Emacs because Emacs already knows about its content:
        save-interprogram-paste-before-kill nil
        ;; Register an handler for GPaste Update signals so we can
        ;; immediately update the `kill-ring':
        gpastel--dbus-object (gpastel-dbus-call #'dbus-register-signal
                                                "Update"
                                                #'gpastel--update-handler))
  (setup-exwm-input-set-keys
   ("\C-x\C-y" browse-kill-ring)))

(require 'magit-popup)

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

(defun gnome-screenshot (args)
  (interactive (list (gnome-screenshot-arguments)))
  (exec! (list* "gnome-screenshot" args)))

(setup-exwm-input-set-keys
 ("<print>" gnome-screenshot-popup))
