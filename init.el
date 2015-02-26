(defun load-relative (&rest paths)
  (dolist (path paths)
    (let ((init-path (file-name-directory load-file-name)))
      (load (concat init-path path)))))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")))

(load-relative
 "bootstrap/base.el"
 "bootstrap/deps-el-get.el"
 "bootstrap/c.el"
 "bootstrap/lisp.el"
 "bootstrap/org.el"
 "bootstrap/hooks.el"
 "bootstrap/ctlz.el"
 "bootstrap/keybindings.el"
 "bootstrap/misc.el"
 "bootstrap/utils.el"
 "bootstrap/window.el"
 "bootstrap/w3-services.el")

(add-exec-paths
 (list "/usr/local/bin"
       (expand-file-name "~/bin")))

(try-require 'ibuffer)
(try-require 'vc)
(try-require 'longlines)
(try-require 'adaptive-wrap)
(try-require 'font-lock)
(try-require 'calendar)
(try-require 'appt)
(try-require 'shell)
(try-require 'rcompile)
(try-require 'info)
(try-require 'buffer-move)
(try-require 'vc-git)
(try-require 'magit)

(try-require 'js2-mode)
(try-require 'guide-key)
(try-require 'dictionary)

(display-time)
(appt-activate 1)
(winner-mode 1)
(set-default 'truncate-lines t)
(set-default 'truncate-partial-width-windows t)
(set-default 'line-move-visual nil)

(setq server-use-tcp t)
(server-start)

(edit-server-start)

(load-file-if-exists "~/.personal.el")

(when (file-exists-p (expand-file-name "~/.bash_profile"))
  (setq explicit-bash-args '("--login" "--init-file" "~/.bash_profile" "-i")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(smtpmail-smtp-server "smtp.gmail.com" t)
 '(smtpmail-smtp-service 25 t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-nick-default-face ((t (:foreground "blue" :weight bold))))
 '(erc-timestamp-face ((t (:foreground "blue" :weight bold))))
 '(fringe ((t nil)))
 '(jabber-chat-prompt-foreign ((t (:weight bold))))
 '(jabber-chat-prompt-local ((t (:foreground "blue"))))
 '(jabber-chat-prompt-system ((t (:foreground "red"))))
 '(jabber-title-medium ((t (:height 1.5)))))
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
