(defun rk--show-elfeed-buffer (buf)
  (display-buffer-same-window buf nil))

(defvar rk--elfeed-entry-link-map nil)

(use-package elfeed
  :ensure t
  :config
  ;; elfeed-feeds are set in ~/.personal.el
  (setq elfeed-show-entry-switch 'rk--show-elfeed-buffer
        rk--elfeed-entry-link-map '(("https?://\\(www.\\)*reddit.com\\(.*\\)" .
                                     "https://old.reddit.com\\2")))
  :bind
  (:map elfeed-search-mode-map
        ("U" . elfeed-update)))

(defun rk--remap-elfeed-entry-link (link)
  (when link
    (let ((re (cl-find-if (lambda (re)
                            (string-match re link))
                          (mapcar 'car rk--elfeed-entry-link-map))))
      (if re
          (let ((rep (cdr (assoc re rk--elfeed-entry-link-map))))
            (replace-regexp-in-string re rep link))
        link))))

;; overrides
;;
(defun elfeed-show-visit (&optional use-generic-p)
  "Visit the current entry in your browser using `browse-url'.
If there is a prefix argument, visit the current entry in the
browser defined by `browse-url-generic-program'."
  (interactive "P")
  (let ((link (rk--remap-elfeed-entry-link
               (elfeed-entry-link elfeed-show-entry))))
    (when link
      (message "Sent to browser: %s" link)
      (if use-generic-p
          (browse-url-generic link)
        (browse-url link)))))

(provide 'init-elfeed)
