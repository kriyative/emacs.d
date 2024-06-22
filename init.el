(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(setq byte-compile-warnings '((not cl-functions)))
(require 'cl)
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
(put 'narrow-to-region 'disabled nil)
