(defun setup-info-faces ()
  ;;(set-face-foreground 'Info-title-1-face "black")
  ;; (set-face-foreground 'Info-title-2-face "black")
  (set-face-foreground 'info-xref nil))

(eval-after-load 'info
  '(setup-info-faces))

(set-terminal-coding-system 'unix)
(mapcar (lambda (face)
          (set-face-background face "white")
          (set-face-inverse-video-p face nil)
          (set-face-bold-p face t))
        '(highlight))
(set-face-background 'region "cyan")
(set-face-foreground 'region nil)
(normal-erase-is-backspace-mode 0)
(menu-bar-mode -1)

(set-default 'fringes-outside-margins t)

(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)
