(rk-el-get-bundles alphapapa/plz.el
                   kriyative/ement.el
                   ts)

(use-package ement
  :config
  (require 'ement-room-list)
  (require 'ement-notify)
  (define-key ement-room-mode-map (kbd "<C-return>") 'rk-ement-room-compose)
  (setq ement-uri-proxy "http://localhost:8008"
        ement-room-message-format-spec "%S%L%B%r%R%t"
        ;; ement-save-session t
        )
  (copy-face 'ement-room-message 'ement-room-self-message))

(require 'ement-room)
(ement-room-define-event-formatter ?S
  "Sender display name."
  (ignore session)
  (let ((ement-room-left-margin-width 12)
        (sender (ement-room--format-user (ement-event-sender event) room)))
    (when (and ement-room-left-margin-width
               (< (string-width sender) ement-room-left-margin-width))
      ;; Using :align-to or :width space display properties doesn't
      ;; seem to have any effect in the margin, so we make a string.
      (setf sender (concat sender
                           (make-string (- ement-room-left-margin-width
                                           (string-width sender))
                                        ? ))))
    ;; NOTE: I'd like to add a help-echo function to display the
    ;; sender ID, but the Emacs manual says that there is currently no
    ;; way to make text in the margins mouse-sensitive.  So
    ;; `ement-room--format-user' returns a string propertized with
    ;; `help-echo' as a string.
    (concat sender "​")))

(ement-room-define-event-formatter ?O
  "Room display name."
  (ignore event session)
  (let ((room-name (propertize (or (ement-room-display-name room)
                                   (ement-room--room-display-name room))
                               'face 'ement-room-name
                               'help-echo (or (ement-room-canonical-alias room)
                                              (ement-room-id room)))))
    ;; HACK: This will probably only be used in the notifications
    ;; buffers, anyway.
    (when ement-notify-limit-room-name-width
      (setf room-name (format (format "%%-%ds"
                                      ement-notify-limit-room-name-width)
                              (truncate-string-to-width room-name
                                                        ement-notify-limit-room-name-width
                                                        nil
                                                        nil
                                                        ement-room-ellipsis))))
    room-name))

(defvar rk--ement-user-id-re
  (rx bos "@" (group (1+ (not (any ":"))))            ; Username
      ":" (group (optional (1+ (not (any blank))))))) ; Server name

(defun rk--ement-get-credentials (&optional user-id)
  (let* ((user-id (or user-id (read-string "User ID: ")))
         (match (string-match rk--ement-user-id-re user-id))
         (host (and match (match-string 2 user-id)))
         (found (when (fboundp 'auth-source-search)
                  (first (auth-source-search
                          :user user-id
                          :host host
                          :max 1
                          :require '(:secret))))))
    (list :user-id user-id
          :password (or (let ((secret (plist-get found :secret)))
                          (if (functionp secret)
                              (funcall secret)
                            secret))
                        (password-read
                         (format "Matrix password for %s:" user-id)
                         (concat "matrix:" user-id))))))

(defun rk-ement-connect ()
  (interactive)
  (apply 'ement-connect
         :uri-prefix "http://localhost:8008"
         (rk--ement-get-credentials)))

(defun rk--generate-jitsi-link ()
  (format "https://meet.jit.si/kriyative%s"
          (time-convert nil 'integer)))

;; (rk--generate-jitsi-link)

(defun rk-ement-insert-jitsi-link ()
  (interactive)
  (ement-room-send-message ement-room
                           ement-session
                           :body (rk--generate-jitsi-link)))

(defun rk--ement-body-format-org-md (body)
  (with-temp-buffer
    (insert body)
    (org-mode)
    (let ((org-export-show-temporary-export-buffer nil))
      (org-md-export-as-markdown)
      (delete-region (point-min) (point-max))
      (with-current-buffer "*Org MD Export*"
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun rk--ement-body-format-org-html (body)
  (with-temp-buffer
    (insert body)
    (org-mode)
    (let ((org-export-show-temporary-export-buffer nil))
      (org-html-export-as-html nil nil nil :body-only)
      (delete-region (point-min) (point-max))
      (with-current-buffer "*Org HTML Export*"
        (goto-char (point-min))
        (while (search-forward-regexp "<pre .*>" nil t)
          (replace-match "<pre><code>\n"))
        (goto-char (point-min))
        (while (search-forward-regexp "</pre>" nil t)
          (replace-match "\n</code></pre>"))
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun rk--ement-body-format-md-html (body)
  (let ((md-export-buf "*Markdown HTML Export*"))
    (with-temp-buffer
      (insert body)
      (markdown md-export-buf)
      (with-current-buffer md-export-buf
        (goto-char (point-min))
        (while (search-forward-regexp "<code>\n" nil t)
          (replace-match "<pre><code>"))
        (goto-char (point-min))
        (while (search-forward-regexp "</code>" nil t)
          (replace-match "</code></pre>\n"))
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun rk-ement-room-send-message-send ()
  (interactive)
  (let* ((body (buffer-substring-no-properties (point-min) (point-max)))
         (formatted-body (rk--ement-body-format-org-html body)))
    (ement-room-send-message ement-room
                             ement-session
                             :body body
                             :formatted-body formatted-body
                             :formatted-body-format "org.matrix.custom.html"
                             :replying-to-event ement-room-replying-to-event)
    (delete-region (point-min) (point-max))
    (quit-window)))

(defun rk-ement-room-send-message-cancel ()
  (interactive)
  (delete-region (point-min) (point-max))
  (quit-window))

(defun rk-ement-room-send-message-composer-insert-@ ()
  (interactive)
  (let* ((username (completing-read-default "Username: "
                                            ement-users
                                            nil
                                            t))
         (user (gethash username ement-users))
         (display-name (when user
                         (ement-room--user-display-name user ement-room))))
    (insert (format "[[https://matrix.to/#/%s][%s]]" username display-name))))

(defun rk-ement-room-compose ()
  (interactive)
  (cl-assert ement-room)
  (cl-assert ement-session)
  (ement-room-with-typing
    (let ((buf (get-buffer-create
                (format "*Ement Compose: %s*"
                        (ement-room-display-name ement-room))))
          (local-ement-session ement-session)
          (local-ement-room ement-room)
          (local-ement-room-replying-to-event ement-room-replying-to-event))
      (with-current-buffer buf
        (visual-line-mode)
        (org-mode)
        (local-set-key (kbd "@") 'rk-ement-room-send-message-composer-insert-@)
        (local-set-key (kbd "\C-c \C-c") 'rk-ement-room-send-message-send)
        (local-set-key (kbd "\C-c \C-k") 'rk-ement-room-send-message-cancel)
        (setq-local ement-session local-ement-session
                    ement-room local-ement-room
                    ement-room-replying-to-event local-ement-room-replying-to-event)
        (pop-to-buffer buf)
        (message "Press C-c C-c to send, C-c C-k to cancel"))))
  (ement-room-scroll-up-mark-read))

(defvar pantalaimon-proc nil)

(defun run-pantalaimon ()
  (interactive)
  (let ((buf "*pantalaimon*"))
    (unless (and pantalaimon-proc
                 (process-live-p pantalaimon-proc))
      (setq pantalaimon-proc (start-process "pantalaimon"
                                            buf
                                            "pantalaimon"))
      (with-current-buffer buf
        (comint-mode)))))

(run-pantalaimon)
