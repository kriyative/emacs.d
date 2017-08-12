(el-get-bundle rudel)

;;;;;;;;;;;;;;;; rudel ;;;;;;;;;;;;;;;;

(defun setup-rudel ()
  (require 'rudel-obby-server)
  (require 'rudel-interactive)
  (require 'rudel-transport)
  (require 'rudel-protocol)
  (require 'rudel-backend)
  (require 'rudel-obby)
  (require 'rudel-socket))

(defvar *current-rudel-session* nil)

(defun rudel-start-host ()
  (interactive)
  (let ((info (list :address rudel-session-host
                    :port rudel-session-port
                    :transport-backend rudel-session-transport
                    :protocol-backend rudel-session-protocol)))
    (setq *current-rudel-session* (rudel-host-session info))))

(defun rudel-remove-user-by-name (username)
  (when *current-rudel-session*
    (rudel-remove-user *current-rudel-session*
                       (rudel-find-user *current-rudel-session* username))))

(defun rudel-join ()
  (interactive)
  (let ((info (list :host rudel-session-host
                    :address rudel-session-host
                    :port rudel-session-port
                    :encryption nil
                    :global-password ""
                    :user-password ""
                    :transport-backend rudel-session-transport
                    :protocol-backend rudel-session-protocol)))
    (rudel-join-session info)))

(use-package rudel
  :init (setq rudel-session-host "localhost"
              rudel-session-port 6522
              rudel-session-transport '(tcp .
                                            [object rudel-tcp-backend
                                                    "tcp"
                                                    (0 2)
                                                    (listen connect)])
              rudel-session-protocol '(obby .
                                            [object rudel-obby-backend
                                                    "obby"
                                                    (0 3)
                                                    (join host
                                                          change-color
                                                          track-subscriptions
                                                          encrypt)]))
  :config (setup-rudel))

