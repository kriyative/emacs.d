(defun load-relative (&rest paths)
  (dolist (path paths)
    (let ((init-path (file-name-directory load-file-name)))
      (load (concat init-path path)))))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/packages/"))

(load-relative "bootstrap/deps-el-get.el")

(require 'efun-base)
(require 'efun-cmds)

(load-relative
 "bootstrap/base.el"
 "bootstrap/init-hooks.el"
 "bootstrap/init-prog-modes.el"
 "bootstrap/init-org.el"
 "bootstrap/init-keys.el"
 "bootstrap/plat.el"
 "bootstrap/window.el"
 "bootstrap/commands.el")

(try-require 'ibuffer)
(try-require 'vc)
(try-require 'adaptive-wrap)
(try-require 'font-lock)
(try-require 'calendar)
(try-require 'appt)
(try-require 'shell)
(try-require 'info)
(try-require 'buffer-move)
(try-require 'vc-git)
(try-require 'magit)
(try-require 'js2-mode)
(try-require 'guide-key)
(try-require 'dictionary)
(try-require 'pwcrypt)
(try-require 'dired-x)
(try-require 'epa-file)
(try-require 'emms-setup)

(display-time)
(appt-activate 1)
(winner-mode 1)
(set-default 'truncate-lines t)
(set-default 'truncate-partial-width-windows t)
(set-default 'line-move-visual nil)

;; raise the limit for bindings and unwind-protect contexts
(setq max-specpdl-size 5000)

(setq server-use-tcp t)
(ignore-errors
  (server-start))
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
 '(jabber-title-medium ((t (:height 1.5))))
 '(mu4e-header-highlight-face ((t (:background "grey" :underline t :weight bold))))
 '(mu4e-unread-face ((t (:inherit font-lock-keyword-face :foreground "blue" :weight bold)))))
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
