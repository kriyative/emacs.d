(require 'slack)

(eval-after-load 'slack
  '(progn
     (setq slack-buffer-emojify nil
	   slack-prefer-current-team t
	   ;; lui-prompt-string "=> "
	   )
     (slack-register-team :name "emacs-slack"
			  :default t
			  :client-id slack-client-id
			  :client-secret slack-client-secret
			  :token slack-token
			  :subscribed-channels '())))

