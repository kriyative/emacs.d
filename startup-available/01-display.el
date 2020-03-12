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
         (height (* (second mm-size) convert)))
    (list :width (round width)
          :height (round height)
          :diagonal (round (sqrt (+ (* width width) (* height height)))))))

;; (plist-get (rk--display-dimensions-inches) :diagonal)

(defun rk--display-dpi ()
  (when (eq window-system 'x)
    (let ((dim (rk--display-dimensions-inches)))
      (round (/ (x-display-pixel-width)
                (plist-get dim :width))))))

;; (rk--display-dpi)

(defvar *rk--current-display-dpi* (rk--display-dpi))

(defun rk--optimum-font-size ()
  (let ((dpi (rk--display-dpi)))
    (cond
     ((< 170 dpi) 14)
     ((or (< 1920 (x-display-pixel-width)) (< 150 dpi)) 13)
     (t 11))))

(defun rk--optimum-font ()
  (let ((dpi (rk--display-dpi)))
    (cond
     ((< 170 dpi) 14)
     ((or (< 1920 (x-display-pixel-width)) (< 150 dpi)) 13)
     (t 11))))

;; (rk--optimum-font-size)

(defun rk--x-set-font (font-family &optional font-size)
  (let* ((font-size (or font-size (rk--optimum-font-size)))
         (x-font (concat font-family " " (prin1-to-string font-size))))
    (set-frame-font x-font t t)
    (setq default-frame-alist `((font . ,x-font)))))

;; to deal with tty mode emacsclient connections
(when (eq window-system 'x)
  (rk--x-set-font "DejaVu Sans Mono Book"))

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

(use-package alert)

(defun rk--compilation-end-notifier (buffer status)
  (rk--x-notify status (concat "emacs - " (buffer-name buffer))))

(push 'rk--compilation-end-notifier compilation-finish-functions)

(defun rk-x-caps-lock-control ()
  (interactive)
  (call-process "setxkbmap" nil nil nil "-option" "ctrl:nocaps"))

(defun rk--x-set-inputs-parse-devices ()
  (let ((dev-re (concat
                 "^[^A-Za-z]*\\([A-Za-z][-A-Za-z0-9/:,_ ]+"
                 "[-A-Za-z0-9/:,_]\\)"
                 "[ \t]*id=\\([0-9]+\\)"
                 "[ \t]*\\[\\(\\w+\\)"
                 "[ \t]*\\(\\w+\\)"))
        devices)
    (with-temp-buffer
      (call-process "xinput" nil t)
      (goto-char (point-min))
      (while (re-search-forward dev-re nil t)
        (push (list :name (match-string 1)
                    :id (match-string 2)
                    :class (match-string 3)
                    :type (match-string 4))
              devices))
      devices)))

(defun rk--match (item matchers &keys key)
  (cl-find-if (lambda (matcher)
                (let ((value (funcall (or key 'identity) item)))
                  (cond
                   ((stringp matcher) (string-match matcher value))
                   ((functionp matcher) (funcall matcher value))
                   (t (equal matcher value)))))
              matchers))

(defun rk--x-set-inputs-find-devices (device-patterns)
  (cl-remove-if-not (lambda (device)
                      (match device device-patterns
                             :key (lambda (p) (plist-get p :name))))
                    (rk--x-set-inputs-parse-devices)))

(defun rk--x-set-inputs-enabled (device-patterns bool)
  (dolist (device (rk--x-set-inputs-find-devices device-patterns))
    (call-process "xinput"
                  nil
                  nil
                  nil
                  (if bool "--enable" "--disable")
                  (plist-get device :id))))

(defvar rk--x-set-inputs-devices nil)
(setq rk--x-set-inputs-devices
      '("AT Translated Set 2 keyboard"
        "ThinkPad Extra Buttons"
        ;; "SynPS/2 Synaptics TouchPad"
        "TPPS/2 IBM TrackPoint"
        ))

(defun rk-enable-input-devices ()
  (interactive)
  (rk--x-set-inputs-enabled x-set-inputs-devices t))

(defun rk-disable-input-devices ()
  (interactive)
  (rk--x-set-inputs-enabled x-set-inputs-devices nil))

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
(mapcar (lambda (face)
          (set-face-background face "white")
          (set-face-inverse-video-p face nil)
          (set-face-bold-p face t))
        '(highlight))
(set-face-background 'region "cyan")
(set-face-foreground 'region nil)
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

(defun rk-light-theme-white ()
  (interactive)
  (let ((fgcolor "black")
        (bgcolor "white"))
    (when window-system
      (set-foreground-color fgcolor)
      (set-background-color bgcolor)
      (set-face-foreground 'mode-line bgcolor)
      (set-face-background 'mode-line "grey40")
      (set-cursor-color "red")
      (set-face-foreground 'default fgcolor))
    (set-face-foreground 'region fgcolor)
    (set-face-background 'region "lightgrey")
    (set-face-foreground 'minibuffer-prompt fgcolor)
    (set-face-background 'minibuffer-prompt nil)
    (unless window-system
      (set-face-foreground 'mode-line fgcolor)
      (set-face-background 'mode-line bgcolor)
      (set-face-foreground 'menu bgcolor))
    (set-face-background 'isearch "indian red")
    (set-face-foreground 'isearch "white")
    (set-face-background 'fringe "grey99")
    (when (boundp 'font-lock-comment-face)
      (set-face-foreground 'font-lock-comment-face "DimGrey")
      (set-face-foreground 'font-lock-builtin-face "gray20")
      (set-face-foreground 'font-lock-constant-face "DimGrey")
      (set-face-foreground 'font-lock-function-name-face "blue")
      (set-face-foreground 'font-lock-keyword-face "gray20")
      (set-face-foreground 'font-lock-string-face "DimGrey")
      (set-face-foreground 'font-lock-type-face fgcolor)
      (set-face-foreground 'font-lock-variable-name-face fgcolor)
      (set-face-foreground 'font-lock-warning-face "red"))
    (set-face-attribute 'vertical-border nil :background bgcolor :foreground fgcolor)))

(rk-light-theme-white)

(defun rk--darken (color &optional amount)
  (let ((values (color-name-to-rgb color))
        (amount (or amount 0.1)))
    (apply 'format
           "#%02x%02x%02x"
           (mapcar (lambda (v) (* 255 (- 1 amount) v)) values))))

(defun rk--lighten (color &optional amount)
  (let ((values (color-name-to-rgb color))
        (amount (or amount 0.1)))
    (apply 'format
           "#%02x%02x%02x"
           (mapcar (lambda (v) (* 255 (1+ amount) v)) values))))

(defun rk--set-theme-colors (fgcolor bgcolor &rest args)
  (destructuring-bind (&key isearch-fg comment-fg builtin-fg
                            constant-fg function-fg keyword-fg
                            warning-fg)
      args
    (let ((fg-80% (rk--darken fgcolor 0.2))
          (fg-120% (rk--lighten fgcolor 0.2)))
      (when window-system
        (set-foreground-color fgcolor)
        (set-background-color bgcolor)
        (set-face-foreground 'mode-line bgcolor)
        (set-face-background 'mode-line fgcolor)
        (set-cursor-color "red")
        (set-face-foreground 'default fgcolor))
      (set-face-foreground 'region bgcolor)
      (set-face-background 'region fgcolor)
      (unless window-system
        (set-face-foreground 'mode-line fgcolor)
        (set-face-foreground 'menu fgcolor))
      (when (string-match "^2[123]" emacs-version)
        (set-face-background 'isearch (or isearch-fg "indian red"))
        (set-face-foreground 'isearch bgcolor))
      (set-face-foreground 'minibuffer-prompt fgcolor)
      (set-face-background 'fringe bgcolor)
      (when (boundp 'font-lock-comment-face)
        (set-face-foreground 'font-lock-comment-face (or comment-fg fg-80%))
        (set-face-foreground 'font-lock-builtin-face (or builtin-fg fgcolor))
        (set-face-foreground 'font-lock-constant-face (or constant-fg fgcolor))
        (set-face-foreground 'font-lock-function-name-face (or function-fg fg-120%))
        (set-face-foreground 'font-lock-keyword-face (or keyword-fg fg-120%))
        (set-face-foreground 'font-lock-string-face fgcolor)
        (set-face-foreground 'font-lock-type-face fgcolor)
        (set-face-foreground 'font-lock-variable-name-face fgcolor)
        (set-face-foreground 'font-lock-warning-face (or warning-fg "red")))
      (set-face-attribute 'vertical-border nil :background bgcolor :foreground fgcolor)
      (setq cider-stacktrace-frames-background-color bgcolor))))

(defun rk-dark-theme-green ()
  (interactive)
  (rk--set-theme-colors "green3" "black"))

;; (dark-theme-green)

(defun rk-dark-theme-amber ()
  (interactive)
  (rk--set-theme-colors "DarkGoldenrod3" "black"))

;; (dark-theme-amber)
