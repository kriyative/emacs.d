(require 'w3m)
(require 'browse-url)

(defun w3m-browse-url-other-window (url &optional new-session)
  (save-excursion
    (when (one-window-p)
      (split-window-horizontally))
    (other-window 1)
    (let ((w3m-use-tab nil))
      (w3m-browse-url url new-session))))

(defun w3m-mode-hook ()
  (define-key w3m-mode-map "\M-t" 'w3m-copy-buffer))

(add-hook 'w3m-mode-hook 'w3m-mode-hook)

(setq browse-url-browser-function 'w3m-browse-url-other-window)

(defun query-string-encode (s)
  (replace-regexp-in-string "[ ]+" "+" s))

(defun google (q)
  (interactive
   (list (query-string-encode (or (region) (read-string "Google: ")))))
  (browse-url
   (concat "https://www.google.com/search?q=" q)))

(defun ddg ()
  (interactive)
  (browse-url
   (concat "https://duckduckgo.com/?q="
           (query-string-encode (or (region) (read-string "DuckDuckGo: "))))))

(defun mdn (q)
  (interactive
   (list (query-string-encode (or (region) (read-string "MDN: ")))))
  (google (concat "site:developer.mozilla.org " q)))

(defun wikipedia ()
  (interactive)
  (browse-url
   (concat "http://en.wikipedia.org/w/index.php?search="
	   (query-string-encode
            (capitalize (or (region) (read-string "Wikipedia: ")))))))

(defun jabber-chat-html-body (xml-data who mode)
  "Print body for received message in XML-DATA."
  (let* ((message-format (caddar
                          (jabber-xml-get-children
                           (car (jabber-xml-get-children x 'x))
                           'message_format)))
         (htmlp (equal "html" message-format))
         (body (car
                (jabber-xml-node-children
                 (car
                  (jabber-xml-get-children xml-data 'body))))))
    (when body
      (when (eql mode :insert)
	(if (and (> (length body) 4)
		 (string= (substring body 0 4) "/me "))
	    (let ((action (substring body 4))
		  (nick (cond
			 ((eq who :local)
			  (plist-get (fsm-get-state-data jabber-buffer-connection) :username))
			 ((or (jabber-muc-message-p xml-data)
			      (jabber-muc-private-message-p xml-data))
			  (jabber-jid-resource (jabber-xml-get-attribute xml-data 'from)))
			 (t
			  (jabber-jid-displayname (jabber-xml-get-attribute xml-data 'from))))))
	      (insert (jabber-propertize
		       (concat nick
			       " "
			       action)
		       'face 'jabber-chat-prompt-system)))
          (if htmlp
              (let ((beg (point)))
                (insert body)
                (shr-render-region beg (point)))
            (insert
             (jabber-propertize
              body
              'face (case who
                      ((:foreign :muc-foreign) 'jabber-chat-text-foreign)
                      ((:local :muc-local) 'jabber-chat-text-local)))))
          ;; (let ((beg (point))
          ;;       (text (jabber-propertize
          ;;              body
          ;;              'face (case who
          ;;                      ((:foreign :muc-foreign) 'jabber-chat-text-foreign)
          ;;                      ((:local :muc-local) 'jabber-chat-text-local)))))
          ;;   (insert text)
          ;;   (when htmlp
          ;;     (forward-line)
          ;;     (shr-render-region beg (point))))
          ))
      t)))

(add-to-list 'jabber-body-printers 'jabber-chat-html-body)
(add-to-list 'jabber-alert-message-hooks 'jabber-message-display)

