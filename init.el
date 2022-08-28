;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; load bootstrap

(load (expand-file-name "startup/bootstrap.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; load machine specific customizations if present

(load-if-present
 (expand-file-name (concat "private/" (system-name) ".el")
                   user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; load private (potential secrets) customizations if present

(load-if-present
 (expand-file-name "private/.personal.el.gpg"
                   user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; load customization modules

(rk-load-startup-modules)
