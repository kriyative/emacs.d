(el-get-bundle ch11ng/xelb)
(el-get-bundle ch11ng/exwm)

(defun exec! (command)
  (interactive (list (read-shell-command "$ ")))
  (let* ((program-and-args (split-string command))
         (program (car program-and-args))
         (args (cdr program-and-args)))
    (with-current-buffer (get-buffer-create "*Console*")
      (goto-char (point-max))
      (insert "$ " command "\n")
      (apply 'call-process program nil t t args))))

(defun spawn& (command)
  (interactive (list (read-shell-command "$ ")))
  (let* ((program-and-args (split-string command))
         (program (car program-and-args))
         (program-name (file-name-nondirectory program))
         (args (cdr program-and-args)))
    (apply 'start-process program-name program-name program args)))

(global-set-key (kbd "s-!") 'exec!)

(defun my-exwm-init ()
  (exwm-input-set-simulation-keys
   '(([?\C-b] . left)
     ([?\C-f] . right)
     ([?\C-p] . up)
     ([?\C-n] . down)
     ([?\C-a] . home)
     ([?\C-e] . end)
     ([?\M-v] . prior)
     ([?\C-v] . next)
     ([?\C-d] . delete)
     ([?\C-k] . (S-end delete))
     ([?\M-w] . ?\C-c)
     ;; ([?\C-w] . ?\C-x)
     ([?\C-y] . ?\C-v)))
  (spawn& "/usr/lib/gnome-settings-daemon/gsd-xsettings")
  (exec! (concat
                 "synclient"
                 " VertTwoFingerScroll=1"
                 " VertScrollDelta=-111"
                 " HorizScrollDelta=-111"
                 " HorizTwoFingerScroll=1"
                 " TapButton1=1"
                 " TapButton2=3"
                 " TapButton3=2"
                 " PalmDetect=1"))
  (exec! "setxkbmap -option ctrl:nocaps")
  (exec! "xsetroot -cursor_name left_ptr -bg black")
  (spawn& "xscreensaver -no-splash"))

(defun lock-screen ()
  (interactive)
  (exec! "xscreensaver-command -lock"))

(global-set-key (kbd "s-l") 'lock-screen)

(use-package exwm
  :config
  (display-time))

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
  (setq highlight-focus:face 'mode-line
        highlight-focus:face-property :background
        highlight-focus:face-property-value "darkgreen"))

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
