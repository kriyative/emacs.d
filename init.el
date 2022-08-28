;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bootstrap straight.el

(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el"
                                        user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         (concat "https://raw.githubusercontent.com"
                 "/radian-software/straight.el/develop/install.el")
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; load standard customizations

(load custom-file nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; some helper functions

(defun load-if-present (filename &rest load-args)
  "Load FILENAME if it exists on disk using `load' passing on
LOAD-ARGS"
  (when (file-exists-p filename)
    (apply 'load filename load-args)))

(defvar rk-startup-modules '("base" "display" "org" "commands"))

(defun rk-load-startup-modules ()
  (dolist (module rk-startup-modules)
    (load
     (expand-file-name (concat "startup/" module ".el")
                       user-emacs-directory))))

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
