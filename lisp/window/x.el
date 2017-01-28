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
  (if (< 90 (display-dpi)) 14 11))

(let* (
       ;; (font-family "Consolas")
       ;; (font-family "FreeMono")
       (font-family "DejaVu Sans Mono Book")
       ;; (font-family "Ubuntu Mono")
       ;; (font-family "Liberation Mono")
       ;; (font-family "Droid Sans Mono")
       ;; (font-family "Inconsolata")
       (font-size (optimum-font-size))
       (x-font (concat font-family " " (prin1-to-string font-size)))
       )
  (set-frame-font x-font t t)
  (setq default-frame-alist `((font . ,x-font))))

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
