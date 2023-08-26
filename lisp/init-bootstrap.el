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

(load custom-file t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; some helper functions

(defun rk-load-rel (filename &optional base-directory &rest load-args)
  "Load FILENAME relative to BASE-DIRECTORY (defaults to
`USER-EMACS-DIRECTORY`) with additional LOAD-ARGS"
  (let ((base-directory (or base-directory user-emacs-directory)))
    (apply 'load (expand-file-name filename base-directory) load-args)))

(defun rk-require (modules)
  (dolist (module (if (consp modules) modules (list modules)))
    (require module)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; epa setup

(defun rk-find-or-insert (expr insertion)
  (goto-char (point-min))
  (or (re-search-forward expr nil t)
      (progn
        (goto-char (point-max))
        (insert insertion))))

(defun rk-ensure-gpg-loopback-pinentry ()
  (let* ((fname (expand-file-name "~/.gnupg/gpg-agent.conf"))
         (buf (find-file-noselect fname)))
    (unwind-protect
        (with-current-buffer buf
          (dolist (cfg '("allow-emacs-pinentry"
                         "allow-loopback-pinentry"))
            (rk-find-or-insert (format "^[\s ]*%s[\s ]*$" cfg)
                               (format "\n%s" cfg)))
          (save-buffer))
      (kill-buffer buf))))

;; (rk-ensure-gpg-loopback-pinentry)

(use-package epa-file
  :config
  (setq epa-pinentry-mode 'loopback
        epa-file-select-keys nil
        epa-file-encrypt-to nil)
  (epa-file-enable)
  (rk-ensure-gpg-loopback-pinentry))

(provide 'init-bootstrap)
