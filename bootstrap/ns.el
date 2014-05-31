(load-relative "mac.el")

(defun ns-fill-display ()
  (interactive)
  (setq ns-auto-hide-menu-bar t)
  (set-frame-position (selected-frame) 0 0)
  (set-mouse-position (selected-frame) 153 -2)
  (when (= 1200 (display-pixel-height))
    (set-frame-width (selected-frame) 272)
    (set-frame-height (selected-frame) 84)))

(define-key ctl-z-map "x" 'ns-fill-display)
