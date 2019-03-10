(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(load "base")

(load-file-if-exists "~/.personal.el")

(startup-emacs 5)
(load "plat")
(load "window")
