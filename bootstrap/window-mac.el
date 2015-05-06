(require 'alert)

(defun mac-play-sound (media)
  (let ((visible-bell nil))
    (beep)))

(fset 'sys-play-sound 'mac-play-sound)

(defun mac-hide-emacs ()
  (interactive)
  (do-applescript
   (concat "tell application \"System Events\"\n"
	   "set visible of process \"Emacs\" to false\n"
	   "end tell\n")))

(defun mac-control-itunes (command)
  (interactive
   (list (completing-read "Command: "
			  '("activate"
			    "deactivate"
			    "next track"
			    "pause"
			    "play"
			    "previous track"))))
  (do-applescript
   (format "tell application \"iTunes\" to %s\n" command)))

(defun mac-itunes-pause ()
  (interactive)
  (mac-control-itunes "pause"))

(defun mac-itunes-play ()
  (interactive)
  (mac-control-itunes "play"))

(defun growl-notify (message &key app title)
  (call-process "/usr/local/bin/growlnotify" nil nil nil
                "-a" "Emacs.app"
                "-t" "Emacs compilation"
                "-m" status))

(setq alert-default-style 'notifier)
;; this fixes weird precedence bug in el-get
(fmakunbound 'notifications-notify)

(defun nc-notify (message &optional title)
  (alert message :title title))

;; (nc-notify "hello world")

(defun mac-growl-compilation-finish-function (buffer status)
  (when (string-match "^\*compilation\*" (buffer-name buffer))
    (nc-notify status)))

(defun applescript (&rest code)
  (labels
      ((str (expr)
            (with-output-to-string
              (cond
               ((listp expr) (emit expr))
               (t (princ expr)))))
       (strq (expr)
             (cond
              ((listp expr) (with-output-to-string (emit expr)))
              (t (prin1-to-string expr))))
       (emit (expr &optional prefix)
             (case (car expr)
               (list{
                (princ (concat "{ " (join "," (mapcar 'str (cdr expr))) " }")))
               (tell
                (when prefix (princ prefix))
                (princ "tell ")
                (princ (spaced (mapcar 'strq (mklist (nth 1 expr)))))
                (princ "\n")
                (dolist (subexpr (subseq expr 2))
                  (when prefix (princ prefix))
                  (emit subexpr (concat prefix "  ")))
                (when prefix (princ prefix))
                (princ "end tell\n"))
               (t
                (princ (spaced (mapcar 'strq expr)))
                (princ "\n")))))
    (let ((script (with-output-to-string (dolist (expr code) (emit expr)))))
      (message script)
      (do-applescript script))))
		  
(defun mac-dictionary-search ()
  (interactive)
  (let ((term (term-at-point-or-read)))
    (message "looking up %s" term)
    (if term
	(applescript
	 `(tell (application "Dictionary") (activate))
	 `(tell (application "System Events")
		(tell (process "Dictionary")
		      (set frontmost to true)
		      (keystroke "F" using (list{ "command down" "option down"))
		      (keystroke ,term)
		      (keystroke return))))
      (message "lookup what?"))))

(unless (fboundp 'mac-toggle-max-window)
  (defun mac-toggle-max-window ()
    (interactive)
    (let ((fullscreenp (eq (frame-parameter nil 'fullscreen) 'fullboth)))
      (set-frame-parameter nil 'fullscreen (unless fullscreenp 'fullboth)))))

(defun mac-move-mouse (x y)
  (call-process "cliclick" nil nil nil (format "m:%d,%d" x y)))

(defun mac-mouse-top-center ()
  (interactive)
  (let ((sx (/ (display-pixel-width) 2))
        (sy 0))
    (mac-move-mouse sx sy)))

(push 'mac-growl-compilation-finish-function compilation-finish-functions)

(setq initial-frame-alist
      `((vertical-scroll-bars)
        (scroll-bar-width)
        (top . 22)
        (left . 0)
        (width . 181)
        (height . 50))
      default-frame-alist initial-frame-alist)
;; (add-hook 'term-setup-hook 'mac-toggle-max-window)

;;........1.........2.........3.........4.........5.........6.........7.........8.........9
;;23456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
;; (set-default-font "Consolas 18")
(set-default-font "Monaco 12")
;; (set-default-font "Menlo 12")
;; (set-default-font "Andale Mono 10")
;; (set-default-font "Inconsolata 13")
;; (set-default-font "Inconsolata 18")
(global-set-key "\M-h" 'mac-hide-emacs)
(global-set-key "\M- " 'just-one-space)
(global-set-key '[C-tab] 'other-window)

(setq mac-pass-command-to-system nil)
(set-fringe-mode '(5 . 5))
(set-default 'fringe-indicator-alist
             '((truncation . empty-line)
               (continuation left-curly-arrow right-curly-arrow)
               (overlay-arrow . right-triangle)
               (up . up-arrow)
               (down . down-arrow)
               (top top-left-angle top-right-angle)
               (bottom bottom-left-angle bottom-right-angle top-right-angle top-left-angle)
               (top-bottom left-bracket right-bracket top-right-angle top-left-angle)
               (empty-line . empty-line)
               (unknown . question-mark)))
(scroll-bar-mode -1)
;; (display-time-mode -1)
(setq mac-command-modifier 'meta
      x-select-enable-clipboard t
      one-buffer-one-frame-mode nil
      mac-autohide-menubar-on-maximize nil)

(add-exec-paths '("/usr/local/MacGPG2/bin"))

;; uncomment if mouse should move to top center automatically when
;; emacs frame takes focus
;; (remove-hook 'focus-in-hook 'mac-mouse-top-center)
