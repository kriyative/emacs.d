(require 'alert)

(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-set-key '[C-tab] 'other-window)

;;........1.........2.........3.........4.........5.........6.........7.........8.........9
;;23456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
(defvar x-font nil)
;; (setq x-font "Liberation Mono 10")

(defun display-dpi ()
  (let* ((monitor-attribs (display-monitor-attributes-list))
         (attrib1 (first monitor-attribs))
         (mm-size (cdr (assq 'mm-size attrib1)))
         (width-inches (* (first mm-size) 0.039)))
    (round (/ (x-display-pixel-width) width-inches))))

;; (display-dpi)

(defun optimum-font-size ()
  (if (< 100 (display-dpi)) 14 11))

(defun x-set-font (font-family &optional font-size)
  (let* ((font-size (or font-size (optimum-font-size)))
         (x-font (concat font-family " " (prin1-to-string font-size))))
    (set-frame-font x-font t t)
    (setq default-frame-alist `((font . ,x-font)))))

(x-set-font "DejaVu Sans Mono Book" 9)

;; (x-set-font "Consolas")
;; (x-set-font "Noto Mono" 9)
;; (x-set-font "DejaVu Sans Mono Book" 13)
;; (x-set-font "Andale Mono" 10)
;; (x-set-font "Bitstream Vera Sans Mono" 10)
;; (x-set-font "FreeMono" 10)
;; (x-set-font "Ubuntu Mono" 10)
;; (x-set-font "Liberation Mono" 10)
;; (x-set-font "Droid Sans Mono" 10)
;; (x-set-font "Inconsolata" 11)
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

(defun x-notify (message &optional title)
  (let ((alert-default-style (if (emacs-focused-p) 'message 'notifications)))
    (alert message :title (or title (concat "emacs@" system-name)))))

;; (x-notify "hello")

(defun compilation-end-notifier (buffer status)
  (x-notify status (concat "emacs - " (buffer-name buffer))))

(push 'compilation-end-notifier compilation-finish-functions)

(mouse-avoidance-mode 'none)
