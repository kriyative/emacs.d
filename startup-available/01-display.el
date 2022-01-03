(rk-el-get-bundles kriyative/kriyative-emacs-themes)
(rk-require-packages unicode-fonts)

(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

(defun rk--set-window-width* (width pixelwisep)
  (let* ((w (selected-window))
         (delta (- width (window-width w pixelwisep))))
    (window-resize w delta t nil pixelwisep)))

(defun rk-set-window-width (&optional width)
  (interactive "nWidth: ")
  (rk--set-window-width* width nil))

(defun rk-set-window-pixel-width (&optional width)
  (interactive "nWidth: ")
  (rk--set-window-width* width t))

(when window-system
  (scroll-bar-mode -1)
  (mouse-avoidance-mode 'none))
(menu-bar-mode -1)
(global-set-key '[C-tab] 'rk-next-window)
(global-set-key '[C-iso-lefttab] 'rk-previous-window)

;;........1.........2.........3.........4.........5.........6.........7.........8.........9
;;23456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
(defvar x-font nil)

(defun rk--display-dimensions-inches ()
  (let* ((monitor-attribs (display-monitor-attributes-list))
         (attrib1 (first monitor-attribs))
         (mm-size (cdr (assq 'mm-size attrib1)))
         (convert 0.039)
         (width (* (first mm-size) convert))
         (height (* (second mm-size) convert))
         (geometry (cdr (assq 'geometry attrib1))))
    (list :width (round width)
          :height (round height)
          :diagonal (round (sqrt (+ (* width width) (* height height))))
          :geometry geometry)))

;; (plist-get (rk--display-dimensions-inches) :diagonal)

(defun rk-display-pixel-width ()
  (nth 2 (plist-get (rk--display-dimensions-inches) :geometry)))

(defun rk--display-dpi ()
  (when (eq window-system 'x)
    (let ((dim (rk--display-dimensions-inches)))
      (round (/ (nth 2 (plist-get dim :geometry))
                (plist-get dim :width))))))

;; (rk--display-dpi)

(defvar *rk--current-display-dpi* (rk--display-dpi))

(defun rk--optimum-font-size ()
  (let ((dpi (rk--display-dpi)))
    (cond
     ((< 170 dpi) 16)
     ((or (< 1920 (rk-display-pixel-width)) (< 150 dpi)) 13)
     (t 11))))

;; (rk--optimum-font-size)

(defvar *rk--font* "DejaVu Sans Mono Book")
(defvar *rk--font-size* 0)

(defun rk--x-set-font (font-family &optional font-size)
  (let* ((font-size (or font-size (rk--optimum-font-size)))
         (x-font (concat font-family " " (prin1-to-string font-size))))
    (set-frame-font x-font t t)
    (setq *rk--font* font-family
          *rk--font-size* font-size)
    (setq default-frame-alist `((font . ,x-font)))))

;; to deal with tty mode emacsclient connections
(when (eq window-system 'x)
  (rk--x-set-font "DejaVu Sans Mono Book"))

;; (rk--x-set-font "Iosevka Term Slab")
;; (rk--x-set-font "Hack")
;; (rk--x-set-font "Fira Code")

;; (rk--x-set-font "DejaVu Sans Mono Book" 18)

(defun rk--x-zoom-in ()
  (interactive)
  (rk--x-set-font *rk--font* (1+ *rk--font-size*)))

(defun rk--x-zoom-out ()
  (interactive)
  (rk--x-set-font *rk--font* (max 9 (- *rk--font-size* 1))))

(defun rk--x-zoom-reset ()
  (interactive)
  (rk--x-set-font *rk--font*))

(rk-bind-keys
 '(("+" rk--x-zoom-in)
   ("-" rk--x-zoom-out)
   ("0" rk--x-zoom-reset))
 user-commands-prefix-map)

(defvar *rk--emacs-focused-p* t)
(defun rk--emacs-focused-p ()
  *rk--emacs-focused-p*)

(defun rk--focus-in-hook ()
  (setq *rk--emacs-focused-p* t))

(add-hook 'focus-in-hook 'rk--focus-in-hook)

(defun rk--focus-out-hook ()
  (setq *rk--emacs-focused-p* nil))

(add-hook 'focus-out-hook 'rk--focus-out-hook)

(defun rk--update-default-font ()
  (unless (eq (rk--display-dpi) *rk--current-display-dpi*)
    (setq *rk--current-display-dpi* (rk--display-dpi))
    ;; (x-set-font "Consolas")
    (rk--x-set-font "DejaVu Sans Mono Book")))

(defun rk--window-configuration-change-hook ()
  (when (eq window-system 'x)
    (rk--update-default-font)))

(add-hook 'window-configuration-change-hook 'rk--window-configuration-change-hook)
;; (remove-hook 'window-configuration-change-hook 'rk--window-configuration-change-hook)

(defun rk--x-notify (message &optional title)
  (let ((alert-default-style (if (rk--emacs-focused-p) 'message 'notifications)))
    (alert message :title (or title (concat "emacs@" system-name)))))

(use-package dbus)

;; (run-with-timer 5 nil (lambda () (rk--x-notify "hello")))

;; (rk--x-notify "hello")

(use-package alert
  :config
  (setq alert-default-style 'notifications))

(defun rk--compilation-end-notifier (buffer status)
  (rk--x-notify status (concat "emacs - " (buffer-name buffer))))

(push 'rk--compilation-end-notifier compilation-finish-functions)

(defun rk-x-caps-lock-control ()
  (interactive)
  (call-process "setxkbmap" nil nil nil "-option" "ctrl:nocaps"))

(defun rk--call-process-post (process-fn post-process-fn)
  (with-temp-buffer
    (funcall process-fn)
    (goto-char (point-min))
    (funcall post-process-fn)))

(defun rk--match (item matchers &keys key)
  (cl-find-if (lambda (matcher)
                (let ((value (funcall (or key 'identity) item)))
                  (cond
                   ((stringp matcher) (string-match matcher value))
                   ((functionp matcher) (funcall matcher value))
                   (t (equal matcher value)))))
              matchers))

(defun rk--xinput-get-devices ()
  (rk--call-process-post
   (lambda ()
     (call-process "xinput" nil t))
   (lambda ()
     (let ((dev-re (concat
                    "^[^A-Za-z]*\\([A-Za-z][-A-Za-z0-9/:,_ ]+"
                    "[-A-Za-z0-9/:,_]\\)"
                    "[ \t]*id=\\([0-9]+\\)"
                    "[ \t]*\\[\\(\\w+\\)"
                    "[ \t]*\\(\\w+\\)"))
           devices)
       (while (re-search-forward dev-re nil t)
         (push (list :name (match-string 1)
                     :id (match-string 2)
                     :class (match-string 3)
                     :type (match-string 4))
               devices))
       devices))))

(defun rk--xinput-find-device (pattern)
  (find-if (lambda (device)
             (string-match pattern (plist-get device :name)))
           (rk--xinput-get-devices)))

;; (rk--xinput-find-device "Synaptics")

(defun rk--xinput (device-ident command &rest args)
  (let* ((device (rk--xinput-find-device device-ident))
         (device-id (if device
                        (plist-get device :id)
                      device-ident)))
    (apply 'call-process
           "xinput"
           nil
           t
           nil
           command
           (append args (list device-id)))))

(defun rk--xinput-find-devices (device-patterns)
  (cl-remove-if-not (lambda (device)
                      (rk--match device device-patterns
                                 :key (lambda (p) (plist-get p :name))))
                    (rk--xinput-get-devices)))

(defun rk--xinput-set-enabled (device-patterns bool)
  (dolist (device (rk--xinput-find-devices device-patterns))
    (call-process "xinput"
                  nil
                  nil
                  nil
                  (if bool "--enable" "--disable")
                  (plist-get device :id))))

(defvar rk--xinput-devices-list nil)
(setq rk--xinput-devices-list
      '("AT Translated Set 2 keyboard"
        "ThinkPad Extra Buttons"
        ;; "SynPS/2 Synaptics TouchPad"
        "TPPS/2 IBM TrackPoint"
        ))

(defun rk-enable-input-devices ()
  (interactive)
  (rk--xinput-set-enabled rk--xinput-devices-list t))

(defun rk-disable-input-devices ()
  (interactive)
  (rk--xinput-set-enabled rk--xinput-devices-list nil))

(defun rk--xinput-list-props (device-name &optional keys)
  (rk--call-process-post
   (lambda ()
     (rk--xinput device-name "--list-props"))
   (lambda ()
     (let (matches
           (re (concat "^[ \t]*\\("
                       (if keys
                           (mapconcat 'identity keys "\\|")
                         "[^(]*")
                       "\\)[ ]*([0-9]+):[ \t]*\\(.*\\)$")))
       (while (re-search-forward re nil t)
         (push (cons (match-string 1) (match-string 2)) matches))
       matches))))

(defun rk--xinput-get-prop (device-name property)
  (cdar (rk--xinput-list-props device-name (list property))))

;; (rk--xinput-get-prop "Synaptics" "Device Enabled")

(defun rk-toggle-touchpad ()
  (interactive)
  (let ((enabledp (equal "1"
                         (rk--xinput-get-prop "Synaptics" "Device Enabled"))))
    (rk--x-set-inputs-enabled '("Synaptics") (not enabledp))))

(defun rk-x-init-roller-mouse ()
  (interactive)
  (call-process "xset" nil nil nil "mouse" "0" "0")
  (call-process "xinput" nil nil nil "--set-prop" "20" "289" "0"))

;;;;;;;;;;;;;;;;

(defun rk--setup-info-faces ()
  ;;(set-face-foreground 'Info-title-1-face "black")
  ;; (set-face-foreground 'Info-title-2-face "black")
  (set-face-foreground 'info-xref nil))

(eval-after-load 'info
  '(rk--setup-info-faces))

(set-terminal-coding-system 'unix)
(normal-erase-is-backspace-mode 0)
(menu-bar-mode -1)
(set-default 'fringes-outside-margins t)

(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(blink-cursor-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
;; (menu-bar-mode -1)
(setq isearch-lazy-highlight nil)
(set-default 'cursor-in-non-selected-windows nil)
(set-default 'mode-line-in-non-selected-windows nil)

(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(setq initial-frame-alist nil)

;; emoji chars
(set-fontset-font t '(#x1f300 . #x1fad0) "Symbola")

(use-package kriyative-emacs-themes
  :config
  (load-theme 'kriyative-light))
