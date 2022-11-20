(use-package erc
  :config
  (setq erc-server "irc.libera.chat"
	erc-port erc-default-port-tls
	erc-hide-list '("JOIN" "PART" "QUIT")
        erc-prompt-for-password nil
        erc-use-auth-source-for-nickserv-password t
        erc-prompt-for-nickserv-password nil))

(use-package erc-services
  :config
  (erc-services-mode 1))

(use-package erc-sasl
  :straight (erc-sasl :type git
                      :host gitlab
                      :repo "psachin/erc-sasl"
                      :build (:not compile))
  :config
  (add-to-list 'erc-sasl-server-regexp-list "irc\\.libera\\.chat"))


;; Redefine/Override the erc-login() function from the erc package, so that
;; it now uses SASL
(defun erc-login ()
  "Perform user authentication at the IRC server. (PATCHED)"
  (erc-log (format "login: nick: %s, user: %s %s %s :%s"
                   (erc-current-nick)
                   (user-login-name)
                   (or erc-system-name (system-name))
                   erc-session-server
                   erc-session-user-full-name))
  (if erc-session-password
      (erc-server-send (format "PASS %s" erc-session-password))
    (message "Logging in without password"))
  (when (and (featurep 'erc-sasl) (erc-sasl-use-sasl-p))
    (erc-server-send "CAP REQ :sasl"))
  (erc-server-send (format "NICK %s" (erc-current-nick)))
  (erc-server-send
   (format "USER %s %s %s :%s"
           ;; hacked - S.B.
           (if erc-anonymous-login erc-email-userid (user-login-name))
           "0" "*"
           erc-session-user-full-name))
  (erc-update-mode-line))

(provide 'init-erc)
