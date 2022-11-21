(use-package mastodon
  :straight (mastodon :type git
                      :host nil
                      :repo "https://codeberg.org/martianh/mastodon.el.git")
  :config
  (setq mastodon-auth-source-file "~/.authinfo.gpg"
        mastodon-tl--show-avatars nil
        mastodon-media--avatar-height 32
        mastodon-tl--enable-relative-timestamps nil
        mastodon-tl--enable-proportional-fonts nil)
  (add-hook 'mastodon-mode-hook 'rk-mastodon-mode-hook)
  ;; from: https://mas.to/@ParetoOptimalDev/109378647927115065
  (add-to-list 'browse-url-handlers
               '("https?://[^/]+/@[^/]+/.*" . 'rk-mastodon-open-at-point)))

(defun rk-mastodon-mode-hook ()
  (visual-line-mode 1)
  (setq-local switch-to-buffer-obey-display-actions t
              display-buffer--same-window-action nil
              bidi-display-reordering nil))

(defun rk-mastodon-open-at-point ()
  "Open the URL at point, or prompt if a URL is not found."
  (interactive)
  (mastodon-url-lookup (or (thing-at-point 'url) (read-string "URL: "))))

(use-package mastodon-tl)

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

(defun mastodon-tl--byline (toot author-byline action-byline &optional detailed-p)
  "Generate byline for TOOT.

AUTHOR-BYLINE is a function for adding the author portion of
the byline that takes one variable.
ACTION-BYLINE is a function for adding an action, such as boosting,
favouriting and following to the byline. It also takes a single function.
By default it is `mastodon-tl--byline-boosted'.

DETAILED-P means display more detailed info. For now
this just means displaying toot client."
  (let* ((created-time
          ;; bosts and faves in notifs view
          ;; (makes timestamps be for the original toot
          ;; not the boost/fave):
          (or (mastodon-tl--field 'created_at
                                  (mastodon-tl--field 'status toot))
              ;; all other toots, inc. boosts/faves in timelines:
              ;; (mastodon-tl--field auto fetches from reblogs if needed):
              (mastodon-tl--field 'created_at toot)))
         (parsed-time (date-to-time created-time))
         (faved (equal 't (mastodon-tl--field 'favourited toot)))
         (boosted (equal 't (mastodon-tl--field 'reblogged toot)))
         (bookmarked (equal 't (mastodon-tl--field 'bookmarked toot)))
         (bookmark-str (if (fontp (char-displayable-p #10r128278))
                           "🔖"
                         "K"))
         (visibility (mastodon-tl--field 'visibility toot)))
    (concat
     ;; Boosted/favourited markers are not technically part of the byline, so
     ;; we don't propertize them with 'byline t', as per the rest. This
     ;; ensures that `mastodon-tl--goto-next-toot' puts point on
     ;; author-byline, not before the (F) or (B) marker. Not propertizing like
     ;; this makes the behaviour of these markers consistent whether they are
     ;; displayed for an already boosted/favourited toot or as the result of
     ;; the toot having just been favourited/boosted.
     (concat (when boosted
               (mastodon-tl--format-faved-or-boosted-byline "B"))
             (when faved
               (mastodon-tl--format-faved-or-boosted-byline "F"))
             (when bookmarked
               (mastodon-tl--format-faved-or-boosted-byline bookmark-str)))
     (propertize
      (concat
       ;; we propertize help-echo format faves for author name
       ;; in `mastodon-tl--byline-author'
       (funcall author-byline toot)
       (cond ((equal visibility "direct")
              (if (fontp (char-displayable-p #10r9993))
                  " ✉"
                " [direct]"))
             ((equal visibility "private")
              (if (fontp (char-displayable-p #10r128274))
                  " 🔒"
                " [followers]")))
       (funcall action-byline toot)
       " "
       ;; TODO: Once we have a view for toot (responses etc.) make
       ;; this a tab stop and attach an action.
       (propertize
        (format-time-string mastodon-toot-timestamp-format parsed-time)
        'timestamp parsed-time
        'display (if mastodon-tl--enable-relative-timestamps
                     (mastodon-tl--relative-time-description parsed-time)
                   parsed-time))
       (when detailed-p
         (let* ((app (alist-get 'application toot))
                (app-name (alist-get 'name app))
                (app-url (alist-get 'website app)))
           (when app
             (concat
              (propertize " via " 'face 'default)
              (propertize app-name
                          'face 'mastodon-display-name-face
                          'follow-link t
                          'mouse-face 'highlight
                          'mastodon-tab-stop 'shr-url
                          'shr-url app-url
                          'help-echo app-url
                          'keymap mastodon-tl--shr-map-replacement)))))
       (propertize "\n" 'face 'default))
      'favourited-p faved
      'boosted-p    boosted
      'bookmarked-p bookmarked
      'byline       t))))

(defun mastodon-tl--insert-status (toot body author-byline action-byline
                                        &optional id parent-toot detailed-p)
  "Display the content and byline of timeline element TOOT.

BODY will form the section of the toot above the byline.
AUTHOR-BYLINE is an optional function for adding the author
portion of the byline that takes one variable. By default it is
`mastodon-tl--byline-author'
ACTION-BYLINE is also an optional function for adding an action,
such as boosting favouriting and following to the byline. It also
takes a single function. By default it is
`mastodon-tl--byline-boosted'.

ID is that of the toot, which is attached as a property if it is
a notification. If the status is a favourite or a boost,
PARENT-TOOT is the JSON of the toot responded to.

DETAILED-P means display more detailed info. For now
this just means displaying toot client."
  (let ((start-pos (point)))
    (insert
     "\n"
     (propertize
      (mastodon-tl--byline toot author-byline action-byline detailed-p)
      'toot-id      (or id              ; for notifications
                        (alist-get 'id toot))
      'base-toot-id (mastodon-tl--toot-id
                     ;; if a favourite/boost notif, get ID of toot responded to:
                     (or parent-toot toot))
      'toot-json    toot
      'parent-toot parent-toot)
     "\n"
     body
     "\n\n"
     "⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯\n")
    (when mastodon-tl--display-media-p
      (mastodon-media--inline-images start-pos (point)))))

(provide 'init-mastodon)
