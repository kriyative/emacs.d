(rk-el-get-bundles
 (emacs-jabber
  :url "git://git.code.sf.net/p/emacs-jabber/git"))

(use-package jabber
  :config
  (add-hook 'jabber-chat-mode-hook 'turn-on-visual-line-mode))
