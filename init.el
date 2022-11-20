(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(require 'init-bootstrap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; load private (potential secrets) customizations if present

(load-if-present
 (expand-file-name "private/.personal.el.gpg"
                   user-emacs-directory))

(rk-require
 'init-base
 'init-display
 'init-org
 'init-commands)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; load machine specific customizations if present

(load-if-present
 (expand-file-name (concat "private/" (system-name) ".el")
                   user-emacs-directory))
