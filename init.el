(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

(load "deps")

;; (use-package efun-base)
(load "base")
(load "setup")
(load "plat")
(load "window")
(load "commands")
(load "overrides")
(load "post")
(load custom-file 'noerror)
