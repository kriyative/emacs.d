(require 'gh-issues)

(setf *current-gh-issues-api* (gh-issues-api "api" :sync nil :cache nil :num-retries 1))
                 
(setf issues (gh-issues-issue-list api "omnypay" "pantheon-modules"))
(object-slots (first (oref issues data)))

;; (id url html-url number state title body user labels assignees
;;  assignee milestone comments pull-request closed-at created-at
;;  updated-at)

(mapcar (lambda (i)
          (let ((ts (slot-value i :created-at)))
            (cons ts
                  (format-time-string "%D %H:%M" (date-to-time ts)))))
        (oref issues data))


(format-time-string "%D %H:%M" (date-to-time "2016-12-08T06:24:42Z"))

([object gh-issues-issue "gh-issues-issue"
         "https://api.github.com/repos/omnypay/pantheon-modules/issues/10"
         "https://github.com/omnypay/pantheon-modules/issues/10"
         10
         "open"
         "Assertions for Payment and Wallet handlers."
         "- [ ]  Assert on Duplicates in wallet.provision-credit-card (or any card)
- [ ]  Assert on Invalid Card Info (Card number etc)
- [ ]  Assert on invalid payment-instrument-id
- [ ]  Assert on wallet.check-balance
- [ ]  Assert on basket.payment and basket.finish-payment
"
         [object gh-user "gh-user"
                 "icylisper"
                 359404
                 nil
                 "https://api.github.com/users/icylisper"]
         nil
         [object gh-user "gh-user"
                 "icylisper"
                 359404 
                 nil
                 "https://api.github.com/users/icylisper"]
         [object gh-issues-milestone "gh-issues-milestone"
                 unbound
                 unbound
                 unbound
                 unbound
                 unbound
                 nil
                 unbound
                 unbound
                 unbound
                 unbound]
         nil
         nil
 "2016-08-29T23:00:17Z" nil])

(defun save-issues ()
  (interactive)
  (github-cache-issues github-issues-current-user github-issues-current-repo
                       (mapcar (lambda (e)
                                 (plist-get (cdr (elt (cadr e) 0)) 'issue))
                               tabulated-list-entries)))
                                  
