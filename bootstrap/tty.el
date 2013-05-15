(defun setup-info-faces ()
  (set-face-foreground 'Info-title-1-face "black")
  (set-face-foreground 'Info-title-2-face "black")
  (set-face-foreground 'info-xref nil))

(eval-after-load 'info
  '(setup-info-faces))

(set-terminal-coding-system 'unix)
(mapcar '(lambda (face)
           (set-face-background face "white")
           (set-face-inverse-video-p face nil)
           (set-face-bold-p face t))
        '(highlight))
(set-face-background 'region "cyan")
(set-face-foreground 'region nil)
(normal-erase-is-backspace-mode 0)
(menu-bar-mode -1)
