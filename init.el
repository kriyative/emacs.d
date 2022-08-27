(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el"
                                        user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(require 'cl)

(load custom-file nil t)

(defvar rk-startup-modules '("base" "display" "org" "commands"))

(load (concat "~/." (system-name) ".el") nil t)
(load "~/.personal.el.gpg" nil t)

(dolist (module rk-startup-modules)
  (message "Loading %s" module)
  (load (expand-file-name (concat "startup/" module ".el")
                          user-emacs-directory)
        nil
        t))
