
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defun rk-start (&optional run-level)
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
  (load "base")
  (load "~/.personal.el" 'noerror)
  (startup-emacs (or run-level
                     (cond
                      ((and window-system
	                    (string-equal "luna" system-name))
                       5)
                      (window-system 4))))
  (load "plat")
  (load "window")
  (load custom-file 'noerror))

(defun rk-start-9emacs ()
  (rk-start 5)
  (x-set-mode-line-color "DarkBlue"))

(cl-case (intern (or (getenv "EMACS_PROFILE") "default"))
  (full (rk-start 6))
  (9emacs (rk-start-9emacs))
  (t (rk-start 5)))
