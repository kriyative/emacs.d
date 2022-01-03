;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(require 'cl)
(require 'epg)
(setq epg-pinentry-mode 'loopback)

(defvar *rk--default-start-level* 3)

(defun rk--start-emacs-level (&optional level)
  (dotimes (i (1+ (or level *rk--default-start-level*)))
    (dolist (f (directory-files "~/.emacs.d/startup"
                                t
                                (format "%02d.*\\.el$" i)))
      (load f))))

(defun rk--start-emacs (&optional run-level)
  (load "~/.personal.el.gpg" 'noerror)
  (rk--start-emacs-level (or run-level 5))
  (load custom-file 'noerror)
  (setq *rk--inited-p* t))

(defun rk-start-emacs-6 ()
  (rk--start-emacs 6))

(defun rk-start-9emacs ()
  (rk--start-emacs 5)
  (rk-set-mode-line-color "DarkBlue"))

(unless (cl-member-if (lambda (s)
                        (string-match "rk-start" s))
                      command-line-args)
  (message "No startup function specified in command line")
  (rk--start-emacs))
