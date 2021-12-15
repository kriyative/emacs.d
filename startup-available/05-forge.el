(rk-el-get-bundles forge)

(defvar *rk-forge-toggle-topic-settings* '((75 . 0) (100 . 50)))

(defun rk-forge-toggle-closed-topics ()
  (interactive)
  (setq forge-topic-list-limit
        (if (equal (car *rk-forge-toggle-topic-settings*)
                   forge-topic-list-limit)
            (cadr *rk-forge-toggle-topic-settings*)
          (car *rk-forge-toggle-topic-settings*)))
  (magit-refresh))

(use-package forge
  :after magit
  :config
  (setq forge-topic-list-limit (car *rk-forge-toggle-topic-settings*))
  (define-key magit-mode-map "\M-c" 'rk-forge-toggle-closed-topics))
