(rk-el-get-bundles
 (emacs-jabber
  :url "git@github.com:kriyative/emacs-jabber.git"))

(cl-defstruct rk--text-section type text)

(defun rk--apply-lang-mode-fontify (lang)
  (ignore-errors
    (let ((mode (intern (concat lang "-mode"))))
      (when (fboundp mode)
        (delay-mode-hooks (funcall mode))
        (font-lock-default-function mode)
        (font-lock-default-fontify-region (point-min) (point-max) nil)))))

(defun rk--split-sections (body)
  (let ((cursor 0) sections slug)
    (while (setf slug (string-match "```\\([^ \t\n]*\\)" body cursor))
      (when (< 0 slug)
        (push (make-rk--text-section :type 'plain
                                     :text (substring body cursor slug))
              sections))
      (let* ((lang (match-string 1 body))
             (slug-start (+ slug 3 (length lang)))
             slug-end)
        (setf slug-end (string-match "```" body slug-start))
        (with-temp-buffer
          (insert (substring body slug-start slug-end))
          (rk--apply-lang-mode-fontify lang)
          (push (make-rk--text-section :type 'code
                                       :text (buffer-string))
                sections))
        (setf cursor (+ slug-end 3))))
    (when (< cursor (length body))
      (push (make-rk--text-section :type 'plain :text (substring body cursor))
            sections))
    (nreverse sections)))

(defun rk--jabber-chat-print-code-body (xml-data who mode)
  "Print body for received message in XML-DATA."
  (let* ((body (car
	        (jabber-xml-node-children
		 (car
		  (jabber-xml-get-children xml-data 'body)))))
         (sections (rk--split-sections body)))
    (if sections
        (when (eql mode :insert)
          (dolist (section sections)
            (if (eq 'code (rk--text-section-type section))
                (insert (rk--text-section-text section))
              (insert (jabber-propertize
		       (rk--text-section-text section)
		       'face (case who
			       ((:foreign :muc-foreign) 'jabber-chat-text-foreign)
			       ((:local :muc-local) 'jabber-chat-text-local)))))))
      (jabber-chat-print-body xml-data who mode))
    t))

(use-package jabber
  :config
  (add-hook 'jabber-chat-mode-hook 'turn-on-visual-line-mode)
  (remove-hook 'jabber-chat-printers 'jabber-chat-print-body)
  (add-hook 'jabber-chat-printers 'rk--jabber-chat-print-code-body)
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
        jabber-console-truncate-lines 1000
        jabber-show-resources nil)
  :bind (:map jabber-chat-mode-map
              ([C-return] . ffap))
  :bind ("C-x C-j" . jabber-global-keymap))
