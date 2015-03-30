(require 'alert)

(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-set-key '[C-tab] 'other-window)

;;........1.........2.........3.........4.........5.........6.........7.........8.........9
;;23456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
(defvar x-font nil)
(setq x-font "Liberation Mono 9")
;; (setq x-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")
;; (setq x-font "-unknown-Liberation Mono-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")
(set-frame-font x-font t t)
(setq default-frame-alist `((font . ,x-font)))

(defun setup-ss ()
  (define-key ctl-z-map "%" 'ss/info))

(eval-after-load 'ss
  '(setup-ss))

(try-require 'ss)

(global-set-key '[C-z space] 'emms-pause)

(require 'emms-setup)
(emms-all)
(emms-default-players)

(defun frame-visible? (&optional frame)
  (eq t (cdr (assoc 'visibility (frame-parameters)))))

(defun x-notify (message &optional title)
  (let ((alert-default-style 'notifications))
    (alert message :title (or title (concat "emacs@" system-name)))))

;; (x-notify "hello")

(defun compilation-end-notifier (buffer status)
  (x-notify status (concat "emacs - " (buffer-name buffer))))

(push 'compilation-end-notifier compilation-finish-functions)

(require 'password-mode)
(require 'org)
(require 'org-passwords)
