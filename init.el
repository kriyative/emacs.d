(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(load "base")
(load-file-if-exists "~/.personal.el")
(startup-emacs
 (cond
  ((and window-system
	(string-equal "luna" system-name))
   5)
  (window-system 4)))
(load "plat")
(load "window")
(load-file-if-exists custom-file 'noerror)
