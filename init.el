(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

(load "deps")

;; (use-package efun-base)
(load "base")
(load "setup")
(load "plat")
(load "window")
(load "commands")
(load "post")
(load custom-settings-file 'noerror)
