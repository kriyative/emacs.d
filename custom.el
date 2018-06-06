(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(blink-cursor-mode nil)
 '(fringe-mode 4 nil (fringe))
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (dired-du oauth2 slack queue htmlize csv-mode clojure-mode adaptive-wrap)))
 '(safe-local-variable-values (quote ((checkdoc-package-keywords-flag))))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25)
 '(tool-bar-mode nil)
 '(window-divider-default-bottom-width 0)
 '(window-divider-default-places t)
 '(window-divider-default-right-width 1)
 ;; '(window-divider-mode t)  ;; exwm
 '(window-divider-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-nick-default-face ((t (:foreground "blue" :weight bold))))
 '(erc-timestamp-face ((t (:foreground "blue" :weight bold))))
 '(jabber-chat-prompt-foreign ((t (:weight bold))))
 '(jabber-chat-prompt-local ((t (:foreground "blue"))))
 '(jabber-chat-prompt-system ((t (:foreground "red"))))
 '(jabber-title-medium ((t (:height 1.5))))
 '(magit-tag ((t (:background "yellow" :foreground "black"))))
 '(mu4e-header-highlight-face ((t (:background "grey" :underline t :weight bold))))
 '(mu4e-unread-face ((t (:inherit font-lock-keyword-face :foreground "blue" :weight bold)))))
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
