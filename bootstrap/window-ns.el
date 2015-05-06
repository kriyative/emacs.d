(load-relative "mac.el")

;; (set-default-font "Menlo 12")
(defun ns-fill-display ()
  (interactive)
  (setq ns-auto-hide-menu-bar nil)
  (set-frame-position (selected-frame) 0 40)
  (set-mouse-position (selected-frame) 153 -2)
  (when (= 1200 (display-pixel-height))
    (set-frame-width (selected-frame) 272)
    (set-frame-height (selected-frame) 80)))

(setq ns-use-native-fullscreen nil)
(define-key ctl-z-map "x" 'mac-toggle-max-window)
