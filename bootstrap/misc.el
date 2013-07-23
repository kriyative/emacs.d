(defun dictionary-init ()
  (load-library "dictionary-init")
  ;; (setq dictionary-server "dict.org")
  (global-set-key "\C-cs" 'dictionary-search)
  (global-set-key "\C-cm" 'dictionary-match-words))

(eval-after-load 'dictionary
  '(dictionary-init))

(eval-after-load 'ibuffer
  '(setq ibuffer-expert t))

(setq inhibit-startup-message t
      inhibit-splash-screen t)

(eval-after-load 'vc
  '(setq vc-mistrust-permissions t
         vc-initial-comment t
         vc-consult-headers nil
         vc-make-backup-files t))

(eval-after-load 'info
  '(add-to-list 'Info-default-directory-list "/usr/local/share/info/"))

(defun man-init ()
  (setenv "MANPATH"
          (join ":"
                '("/usr/local/share/man/"
                  "/usr/share/man/")))
  (setenv "MANWIDTH" "80")
  (add-hook 'Man-mode-hook (lambda () (setq Man-fontify-manpage nil))))
  
(eval-after-load 'man
  '(man-init))

(put 'narrow-to-page 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(eval-after-load 'calendar
  '(iso-calendar))

(defun diary-init ()
  (add-hook 'list-diary-entries-hook 'include-other-diary-files t)
  (when (file-exists-p diary-file) (diary 0)))

(eval-after-load 'diary
  '(diary-init))

(setq backup-inhibited t
      remote-shell-program "ssh"
      truncate-partial-width-windows nil
      ;; appt-message-warning-time 10
      display-time-day-and-date t
      display-time-world-list '(("America/Los_Angeles" "Cupertino")
                                ("America/New_York" "New York")
                                ("Europe/London" "London")
                                ("Europe/Paris" "Paris")
                                ("Asia/Calcutta" "Chennai")
                                ("Asia/Singapore" "Singapore")
                                ("Australia/Sydney" "Sydney")
                                ("Pacific/Auckland" "Auckland"))
      display-time-world-time-format "%a %d %b %R %Z"
      visible-bell 'top-bottom
      url-proxy-services nil
      compilation-scroll-output t
      transient-mark-mode t
      shell-file-name "bash"
      completion-ignored-extensions (nconc completion-ignored-extensions
                                           '(".fasl" ".dfsl" ".x86f" ".err" ".ufasl"
                                             ".DS_Store"))
      max-mini-window-height 1)

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(setq mc-gpg-path (locate-path "gpg" exec-path)
      ispell-program-name (locate-path "aspell" exec-path))

(defun alt-vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 0 name "blame" (if rev (concat  rev)))))

(eval-after-load 'vc-git
  '(fset 'vc-git-annotate-command 'alt-vc-git-annotate-command))

(eval-after-load 'adoc-mode
  '(add-to-list 'auto-mode-alist '("\\.doc$" . adoc-mode)))

;; HipChat via jabber.el
;;
;; from: https://gist.github.com/pufuwozu/4002033
(defun hipchat-join (room)
  (interactive "sRoom name: ")
  (jabber-groupchat-join (jabber-read-account)
                         (concat hipchat-number "_" room "@conf.hipchat.com")
                         hipchat-nickname
                         t))

;; Mention nicknames in a way that HipChat clients will pickup
(defun hipchat-mention (nickname)
  (interactive
   (list (jabber-muc-read-nickname jabber-group "Nickname: ")))
  (insert (concat "@\"" nickname "\" ")))

(eval-after-load 'bbdb
  '(progn
     (bbdb-insinuate-message)
     (bbdb-initialize 'message 'sc)))
