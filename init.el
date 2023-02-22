(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(require 'init-bootstrap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; load private (potential secrets) customizations if present

(rk-load-rel "private/.personal.el.gpg" nil 'noerror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; load core modules

(rk-require
 '(init-base
   init-display
   init-org
   init-commands))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; load machine specific customizations if present

(rk-load-rel (concat "private/" (system-name) ".el") nil 'noerror)
