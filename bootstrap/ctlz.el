(defun ctl-z-help ()
  (interactive)
  (message "Welcome to Ctl-z"))

(defvar ctl-z-map (make-sparse-keymap))

(global-unset-key "\C-z")
(define-prefix-command 'ctl-z-prefix 'ctl-z-map "Ctl-Z")
(define-key global-map "\C-z" 'ctl-z-prefix)

(define-key ctl-z-map "." 'find-tag)
(define-key ctl-z-map "2" '2col-view)
(define-key ctl-z-map "3" '3col-view)
(define-key ctl-z-map "9" 'fill-vertical-panes)
(define-key ctl-z-map "<" 'pop-tag-mark)
(define-key ctl-z-map "?" 'ctl-z-help)
(define-key ctl-z-map "\C-b" 'winner-undo)
(define-key ctl-z-map "\C-f" 'winner-redo)
(define-key ctl-z-map "\C-l" 'bury-buffer)
(define-key ctl-z-map "\C-u" 'browse-url)
;; (define-key ctl-z-map "g" 'toggle-gnus-display)
(define-key ctl-z-map "g" 'toggle-debug-on-error)
(define-key ctl-z-map "j" 'jump-to-register)
(define-key ctl-z-map "t" 'toggle-window-split)
(define-key ctl-z-map "w" 'window-configuration-to-register)
(define-key ctl-z-map [left] 'buf-move-left)
(define-key ctl-z-map [right] 'buf-move-right)
(define-key ctl-z-map "O" 'browse-url-default-macosx-browser)
