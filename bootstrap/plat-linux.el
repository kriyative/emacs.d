(defun setup-ss ()
  (define-key ctl-z-map "%" 'ss/info))

(eval-after-load 'ss '(setup-ss))

(try-require 'ss)

(global-set-key '[C-z space] 'emms-pause)

;; (require 'emms-setup)
;; (emms-all)
;; (emms-default-players)

(require 'password-mode)
(require 'org)
(require 'org-passwords)

