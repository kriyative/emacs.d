(require 'mu4e)

(setq
 mu4e-maildir       "~/Mail"  ;; top-level Maildir
 mu4e-sent-folder   "/sent"   ;; folder for sent messages
 mu4e-drafts-folder "/drafts" ;; unfinished messages
 mu4e-trash-folder  "/omnypay/[Gmail].Trash" ;; trashed messages
 mu4e-refile-folder "/omnypay/[Gmail].All Mail"
 mu4e-maildir-shortcuts
 '(("/omnypay/INBOX"     . ?i)
   ("/omnypay/[Gmail].All Mail"  . ?a)
   ("/omnypay/[Gmail].Sent Mail" . ?s))

 mu4e-get-mail-command "offlineimap -o"
 mu4e-view-prefer-html t
 mu4e-hide-index-messages t
 mu4e-split-view 'vertical
 mu4e-headers-visible-columns (/ (frame-width) 2)

 mu4e-headers-fields '((:human-date .  12) ;; alternatively, use :human-date
                       ;; (:flags      .   6)
                       (:from       .  16)
                       (:subject    .  nil)))

;; (require 'mu4e-multi)
(setq mu4e-multi-account-alist
      '(
        ;; ("personal"
        ;;  (user-mail-address . "personal@someserver.com")
        ;;  (mu4e-drafts-folder . "/personal/Drafts")
        ;;  (mu4e-follow-up-folder . "/personal/FollowUp")
        ;;  (mu4e-hold-folder . "/personal/Hold")
        ;;  (mu4e-refile-folder . "/personal/Archived")
        ;;  (mu4e-sent-folder . "/personal/Sent")
        ;;  (mu4e-trash-folder . "/personal/Trash"))
        ("omnypay"
         (user-mail-address . "ram@omnypay.net")
         (mu4e-drafts-folder . "/omnypay/[Gmail].Drafts")
         ;; (mu4e-follow-up-folder . "/work/FollowUp")
         ;; (mu4e-hold-folder . "/work/Hold")
         (mu4e-refile-folder . "/omnypay/[Gmail].All Mail")
         (mu4e-sent-folder . "/omnypay/[Gmail].Sent Mail")
         (mu4e-trash-folder . "/omnypay/[Gmail].Trash"))))

;; (mu4e-multi-enable)

;; (add-hook 'mu4e-view-mode-hook 'mu4e-view-toggle-html)
;; (remove-hook 'mu4e-view-mode-hook 'mu4e-view-toggle-html)
;; (remove-hook 'mu4e-view-mode-hook 'mu4e-view-hide-cited)
