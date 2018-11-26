;; (package-initialize)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

(load "deps")

;; (use-package efun-base)
(load "base")
(load "setup")
(load "plat")
(load "window")
(load "commands")
;; (load "setup-helm")
(load "setup-ivy")
(load "overrides")
(load "post")
(load custom-file 'noerror)

(if (equal "exwm" (getenv "DESKTOP_SESSION"))
    (load "setup-exwm")
  (load "setup-nonexwm"))
