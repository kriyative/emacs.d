(rk-el-get-bundles
 (emacs-jabber
  :url "git://git.code.sf.net/p/emacs-jabber/git"))

(use-package jabber
  :config
  (add-hook 'jabber-chat-mode-hook 'turn-on-visual-line-mode)
  (setq jabber-auto-reconnect t
        jabber-vcard-avatars-retrieve nil
        jabber-avatar-verbose nil
        jabber-history-enabled t
        jabber-history-muc-enabled t
        jabber-chat-buffer-format "*jabber: %n*"
        jabber-roster-buffer "*jabber*"
        jabber-groupchat-buffer-format "*jabber-groupchat: %n*"
        ;; jabber-roster-line-format " %a %c %-25n %u %-8s  %S"
        jabber-roster-line-format " %c %-25n %u %-8s"
        jabber-show-offline-contacts t
        jabber-chat-local-prompt-format "[%t] %n> "
        jabber-history-size-limit 1024
        jabber-show-resources nil))
