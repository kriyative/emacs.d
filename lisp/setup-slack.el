(el-get-bundle oauth2)
(el-get-bundle websocket)
(el-get-bundle request)
(el-get-bundle circe)
(el-get-bundle alert)
(el-get-bundle yuya373/emacs-slack)

(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify nil
        slack-prefer-current-team t
        ;; lui-prompt-string "=> "
        (setq alert-default-style 'notifier))
  :config
  (slack-register-team :name "emacs-slack"
                       :default t
                       :client-id slack-client-id
                       :client-secret slack-client-secret
                       :token slack-token
                       :subscribed-channels '()))

