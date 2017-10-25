(el-get-bundle ch11ng/xelb)
(el-get-bundle ch11ng/exwm)

(defun exec-process (command)
  (interactive (list (read-shell-command "$ ")))
  (let* ((program-and-args (split-string command))
         (program (car program-and-args))
         (args (cdr program-and-args)))
    (with-current-buffer (get-buffer-create "*Console*")
      (goto-char (point-max))
      (insert "$ " command "\n")
      (apply 'call-process program nil t t args))))

(defun my-exwm-init ()
  (start-process
   "gnome-settings-daemon"
   "gnome-settings-daemon"
   "/usr/lib/gnome-settings-daemon/gsd-xsettings")
  (exec-process (concat
                 "synclient"
                 " VertTwoFingerScroll=1"
                 " VertScrollDelta=-111"
                 " HorizScrollDelta=-111"
                 " HorizTwoFingerScroll=1"
                 " TapButton1=1"
                 " TapButton2=3"
                 " TapButton3=2"
                 " PalmDetect=1"))
  (exec-process "setxkbmap -option ctrl:nocaps")
  (exec-process "xsetroot -cursor_name left_ptr -bg black"))

(use-package exwm
  :config
  (display-time))
(use-package exwm-config
  :config
  (setq exwm-workspace-number 4)
  (add-hook 'exwm-floating-setup-hook #'exwm-layout-hide-mode-line)
  (add-hook 'exwm-floating-exit-hook #'exwm-layout-show-mode-line)
  (exwm-config-default)
  (my-exwm-init))
(use-package exwm-cm
  :config
  (exwm-cm-enable))
(use-package exwm-randr
  :config (exwm-randr-enable))

(global-set-key (kbd "<XF86AudioPlay>") 'emms-pause)
(global-set-key (kbd "<XF86AudioStop>") 'emms-stop)
(global-set-key (kbd "<XF86AudioPrev>") 'emms-previous)
(global-set-key (kbd "<XF86AudioNext>") 'emms-next)

(el-get-bundle flexibeast/pulseaudio-control)
(use-package pulseaudio-control)

(global-set-key (kbd "<XF86AudioLowerVolume>") 'pulseaudio-control-decrease-volume)
(global-set-key (kbd "<XF86AudioRaiseVolume>") 'pulseaudio-control-increase-volume)
(pulseaudio-control-default-keybindings)
