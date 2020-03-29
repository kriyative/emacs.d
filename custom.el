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
    (speed-type dired-du oauth2 slack queue htmlize csv-mode clojure-mode adaptive-wrap)))
 '(safe-local-variable-values
   (quote
    ((eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook
            (quote write-contents-functions)
            (lambda nil
              (delete-trailing-whitespace)
              nil))
           (require
            (quote whitespace))
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (whitespace-style face tabs trailing lines-tail)
     (cider-shadow-cljs-default-options . "browser")
     (cider-default-cljs-repl . shadow)
     (checkdoc-package-keywords-flag))))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25)
 '(tool-bar-mode nil)
 '(window-divider-default-bottom-width 0)
 '(window-divider-default-places t)
 '(window-divider-default-right-width 1)
 '(window-divider-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-quoted ((t (:inherit nil :weight bold))))
 '(erc-nick-default-face ((t (:foreground "blue" :weight bold))))
 '(erc-timestamp-face ((t (:foreground "blue" :weight bold))))
 '(jabber-activity-personal-face ((t (:foreground "yellow" :weight bold))))
 '(jabber-chat-prompt-foreign ((t (:weight bold))))
 '(jabber-chat-prompt-local ((t (:foreground "blue"))))
 '(jabber-chat-prompt-system ((t (:foreground "red"))))
 '(jabber-title-medium ((t (:height 1.5))))
 '(magit-tag ((t (:background "yellow" :foreground "black"))))
 '(mu4e-header-highlight-face ((t (:background "grey" :underline t :weight bold))))
 '(mu4e-unread-face ((t (:inherit font-lock-keyword-face :foreground "blue" :weight bold)))))
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
