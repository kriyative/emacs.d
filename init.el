(defun load-relative (&rest paths)
  (dolist (path paths)
    (let ((init-path (file-name-directory load-file-name)))
      (load (concat init-path path)))))

(load-relative
 "bootstrap/base.el"
 "bootstrap/deps.el"
 "bootstrap/c.el"
 "bootstrap/lisp.el"
 "bootstrap/org.el"
 "bootstrap/hooks.el"
 "bootstrap/ctlz.el"
 "bootstrap/keybindings.el"
 "bootstrap/misc.el"
 "bootstrap/utils.el"
 "bootstrap/window.el")

(add-exec-paths
 (list "/usr/local/bin"
       (expand-file-name "~/bin")))

(require 'ibuffer)
(require 'vc)
(require 'longlines)
(require 'adaptive-wrap)
(require 'font-lock)
(require 'calendar)
(require 'appt)
(require 'shell)
(require 'rcompile)
(require 'info)
(require 'buffer-move)
(require 'vc-git)
(require 'magit)

(require 'w3m)

(require 'clojure-mode)
(require 'clojure-test-mode)
(require 'clojurescript-mode)
(require 'adoc-mode)

(require 'miagi)

(display-time)
(appt-activate 1)
(winner-mode 1)

(setq server-use-tcp t)
(server-start)

(let ((f (expand-file-name "~/.personal.el")))
  (when (file-exists-p f)
    (load f)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t nil))))
