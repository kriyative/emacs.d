(require 'alert)

(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-set-key '[C-tab] 'other-window)

;;........1.........2.........3.........4.........5.........6.........7.........8.........9
;;23456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
(defvar x-font nil)
(setq x-font "Liberation Mono 10")
(let ((x-font
       ;; "Andale Mono 11"
       ;; "Consolas 10"
       ;; "DejaVu Sans Mono Book 11"
       ;; "Droid Sans Mono 10"
       ;; "FreeMono 11"
       ;; "Inconsolata 11"
       ;; "Liberation Mono 10"
       "Ubuntu Mono 12"
       ))
  ;; (setq x-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")
  ;; (setq x-font "-unknown-Liberation Mono-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")
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
