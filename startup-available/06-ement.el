(rk-el-get-bundles alphapapa/plz.el
                   kriyative/ement.el
                   ts)

(use-package ement
  :config
  (require 'ement-room-list)
  (require 'ement-notify)
  (setq ement-uri-proxy "http://localhost:8008"
        ement-room-message-format-spec "%S %L%B%r%R%t"
        ement-formatted-body-formatter-function 'ement--body-format-org-html
        ement-plain-body-formatter-function 'ement--body-format-org-md
        ;; ement-save-session t
        )
  (copy-face 'ement-room-message 'ement-room-self-message))

(defun rk--generate-jitsi-link ()
  (format "https://meet.jit.si/kriyative%s"
          (time-convert nil 'integer)))

;; (rk--generate-jitsi-link)

(defun rk-ement-insert-jitsi-link ()
  (interactive)
  (ement-room-send-message-primordial ement-session
                                      ement-room
                                      (rk--generate-jitsi-link)))

(defun ement--body-format-org-md (body)
  (with-temp-buffer
    (insert body)
    (org-mode)
    (let ((org-export-show-temporary-export-buffer nil))
      (org-md-export-as-markdown)
      (delete-region (point-min) (point-max))
      (with-current-buffer "*Org MD Export*"
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun ement--body-format-org-html (body)
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

(defun ement--body-format-md-html (body)
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

(defvar pantalaimon-proc nil)

(defun run-pantalaimon ()
  (interactive)
  (unless (and pantalaimon-proc
               (process-live-p pantalaimon-proc))
    (setq pantalaimon-proc (start-process "pantalaimon"
                                          "*pantalaimon*"
                                          "pantalaimon"))))

(run-pantalaimon)
