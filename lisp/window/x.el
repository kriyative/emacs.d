(require 'alert)

(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-set-key '[C-tab] 'my-next-window)
(global-set-key '[C-iso-lefttab] 'my-previous-window)

;;........1.........2.........3.........4.........5.........6.........7.........8.........9
;;23456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
(defvar x-font nil)
;; (setq x-font "Liberation Mono 10")

(defun display-dimensions-inches ()
  (let* ((monitor-attribs (display-monitor-attributes-list))
         (attrib1 (first monitor-attribs))
         (mm-size (cdr (assq 'mm-size attrib1)))
         (convert 0.039)
         (width (* (first mm-size) convert))
         (height (* (second mm-size) convert)))
    (list :width (round width)
          :height (round height)
          :diagonal (round (sqrt (+ (* width width) (* height height)))))))

;; (plist-get (display-dimensions-inches) :diagonal)

(defun display-dpi ()
  (when (eq window-system 'x)
    (let ((dim (display-dimensions-inches)))
      (round (/ (x-display-pixel-width)
                (plist-get dim :width))))))

;; (display-dpi)

(defvar *current-display-dpi* (display-dpi))

(defun optimum-font-size ()
  (let ((dpi (display-dpi)))
    (cond
     ((< 170 dpi) 14)
     ((or (< 1920 (x-display-pixel-width)) (< 150 dpi)) 13)
     (t 11))))

(defun optimum-font ()
  (let ((dpi (display-dpi)))
    (cond
     ((< 170 dpi) 14)
     ((or (< 1920 (x-display-pixel-width)) (< 150 dpi)) 13)
     (t 11))))

;; (optimum-font-size)

(defun x-set-font (font-family &optional font-size)
  (let* ((font-size (or font-size (optimum-font-size)))
         (x-font (concat font-family " " (prin1-to-string font-size))))
    (set-frame-font x-font t t)
    (setq default-frame-alist `((font . ,x-font)))))

;; to deal with tty mode emacsclient connections
(when (eq window-system 'x)
  (x-set-font "DejaVu Sans Mono Book"))
;; (x-set-font "Hack" 10)
;; (x-set-font "Consolas" 12)
;; (x-set-font "Inconsolata")
;; (x-set-font "Liberation Mono" 11)
;; (x-set-font "Consolas" 9)
;; (x-set-font "Noto Mono")
;; (x-set-font "DejaVu Sans Mono Book" 11)
;; (x-set-font "Andale Mono")
;; (x-set-font "Bitstream Vera Sans Mono")
;; (x-set-font "FreeMono")
;; (x-set-font "Ubuntu Mono" 14)
;; (x-set-font "Liberation Mono" 9)
;; (x-set-font "Droid Sans Mono" 10)
;; (x-set-font "Inconsolata" 10)
;; (x-set-font "Tlwg Mono" 11)

(defvar *emacs-focused-p* t)
(defun emacs-focused-p ()
  *emacs-focused-p*)

(defun focus-in-hook ()
  (setq *emacs-focused-p* t))

(add-hook 'focus-in-hook 'focus-in-hook)

(defun focus-out-hook ()
  (setq *emacs-focused-p* nil))

(add-hook 'focus-out-hook 'focus-out-hook)

(defun update-default-font ()
  (unless (eq (display-dpi) *current-display-dpi*)
    (setq *current-display-dpi* (display-dpi))
    ;; (x-set-font "Consolas")
    (x-set-font "DejaVu Sans Mono Book")))

(defun window-configuration-change-hook ()
  (when (eq window-system 'x)
    (update-default-font)))


(add-hook 'window-configuration-change-hook 'window-configuration-change-hook)
;; (remove-hook 'window-configuration-change-hook 'window-configuration-change-hook)

(defun x-notify (message &optional title)
  (let ((alert-default-style (if (emacs-focused-p) 'message 'notifications)))
    (alert message :title (or title (concat "emacs@" system-name)))))

;; (x-notify "hello")

(defun compilation-end-notifier (buffer status)
  (x-notify status (concat "emacs - " (buffer-name buffer))))

(push 'compilation-end-notifier compilation-finish-functions)

(mouse-avoidance-mode 'none)

(defun x-caps-lock-control ()
  (interactive)
  (call-process "setxkbmap" nil nil nil "-option" "ctrl:nocaps"))

(defun x-set-inputs-parse-devices ()
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

(defun match (item matchers &keys key)
  (cl-find-if (lambda (matcher)
                (let ((value (funcall (or key 'identity) item)))
                  (cond
                   ((stringp matcher) (string-match matcher value))
                   ((functionp matcher) (funcall matcher value))
                   (t (equal matcher value)))))
              matchers))

(defun x-set-inputs-find-devices (device-patterns)
  (cl-remove-if-not (lambda (device)
                      (match device device-patterns
                             :key (lambda (p) (plist-get p :name))))
                    (x-set-inputs-parse-devices)))

(defun x-set-inputs-enabled (device-patterns bool)
  (dolist (device (x-set-inputs-find-devices device-patterns))
    (call-process "xinput"
                  nil
                  nil
                  nil
                  (if bool "--enable" "--disable")
                  (plist-get device :id))))


(defvar x-set-inputs-devices nil)
(setq x-set-inputs-devices
      '("AT Translated Set 2 keyboard"
        "ThinkPad Extra Buttons"
        ;; "SynPS/2 Synaptics TouchPad"
        "TPPS/2 IBM TrackPoint"
        ))

(defun enable-input-devices ()
  (interactive)
  (x-set-inputs-enabled x-set-inputs-devices t))

(defun disable-input-devices ()
  (interactive)
  (x-set-inputs-enabled x-set-inputs-devices nil))

(defun x-init-roller-mouse ()
  (interactive)
  (call-process "xset" nil nil nil "mouse" "0" "0")
  (call-process "xinput" nil nil nil "--set-prop" "20" "289" "0"))
