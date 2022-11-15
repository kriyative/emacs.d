(use-package mastodon
  :straight (mastodon :type git
                      :host nil
                      :repo "https://codeberg.org/martianh/mastodon.el.git")
  :config
  (setq mastodon-auth-source-file "~/.authinfo.gpg"
        mastodon-tl--show-avatars nil
        mastodon-media--avatar-height 64
        mastodon-tl--enable-relative-timestamps nil
        mastodon-tl--enable-proportional-fonts nil)
  (add-hook 'mastodon-mode-hook 'rk-mastodon-mode-hook))

(defun rk-mastodon-mode-hook ()
  (visual-line-mode 1))

;;; Override 'mastodon-tl--render-text' to render HTML without window
;;; width, and use visual-line-mode to word wrap dynamically. does NOT
;;; work with proportional fonts

(defun mastodon-tl--render-text (string &optional toot)
  "Return a propertized text rendering the given HTML string STRING.

The contents comes from the given TOOT which is used in parsing
links in the text. If TOOT is nil no parsing occurs."
  (when string                    ; handle rare empty notif server bug
    (with-temp-buffer
      (insert string)
      (let ((shr-use-fonts mastodon-tl--enable-proportional-fonts)
            (shr-max-width most-positive-fixnum)
            (shr-width     most-positive-fixnum))
        (shr-render-region (point-min) (point-max)))
      ;; Make all links a tab stop recognized by our own logic, make things point
      ;; to our own logic (e.g. hashtags), and update keymaps where needed:
      (when toot
        (let (region)
          (while (setq region (mastodon-tl--find-property-range
                               'shr-url (or (cdr region) (point-min))))
            (mastodon-tl--process-link toot
                                       (car region) (cdr region)
                                       (get-text-property (car region) 'shr-url)))))
      (buffer-string))))
