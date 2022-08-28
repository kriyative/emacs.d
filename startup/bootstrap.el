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


;;;;;;;;;;;;;;;; epa ;;;;;;;;;;;;;;;;

(defun rk-find-or-insert (expr insertion)
  (goto-char (point-min))
  (or (re-search-forward expr nil t)
      (progn
        (goto-char (point-max))
        (insert insertion))))

(defun rk-ensure-gpg-loopback-pinentry ()
  (let ((fname (expand-file-name "~/.gnupg/gpg-agent.conf")))
    (with-current-buffer (find-file-noselect fname)
      (dolist (cfg '("allow-emacs-pinentry"
                     "allow-loopback-pinentry"))
        (rk-find-or-insert (format "^[\s ]*%s[\s ]*$" cfg)
                           (format "\n%s" cfg)))
      (save-buffer))))

;; (rk-ensure-gpg-loopback-pinentry)

(use-package epa-file
  :config
  (setq epa-pinentry-mode 'loopback)
  (epa-file-enable)
  (rk-ensure-gpg-loopback-pinentry))
