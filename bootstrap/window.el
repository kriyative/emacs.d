(load-relative
 (concat "window-" (symbol-name (or window-system 'tty)) ".el"))

(blink-cursor-mode -1)
(tool-bar-mode -1)
;; (menu-bar-mode -1)
(setq isearch-lazy-highlight nil)
(set-default 'cursor-in-non-selected-windows nil)
(set-default 'mode-line-in-non-selected-windows nil)

(defun light-theme-white ()
  (interactive)
  (let ((fgcolor "black")
        (bgcolor "white"))
    (when window-system
      (set-foreground-color fgcolor)
      (set-background-color bgcolor)
      (set-face-foreground 'mode-line bgcolor)
      (set-face-background 'mode-line fgcolor)
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

(light-theme-white)

(defun darken (color &optional amount)
  (let ((values (color-name-to-rgb color))
        (amount (or amount 0.1)))
    (apply 'format
           "#%02x%02x%02x"
           (mapcar (lambda (v) (* 255 (- 1 amount) v)) values))))

(defun lighten (color &optional amount)
  (let ((values (color-name-to-rgb color))
        (amount (or amount 0.1)))
    (apply 'format
           "#%02x%02x%02x"
           (mapcar (lambda (v) (* 255 (1+ amount) v)) values))))

(defun set-theme-colors (fgcolor bgcolor &rest args)
  (destructuring-bind (&key isearch-fg comment-fg builtin-fg
                            constant-fg function-fg keyword-fg
                            warning-fg)
      args
    (let ((fg-80% (darken fgcolor 0.2))
          (fg-120% (lighten fgcolor 0.2)))
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

(defun dark-theme-green ()
  (interactive)
  (set-theme-colors "green3" "black"))

;; (dark-theme-green)

(defun dark-theme-amber ()
  (interactive)
  (set-theme-colors "DarkGoldenrod3" "black"))

;; (dark-theme-amber)
