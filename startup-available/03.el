(rk-require-packages window-numbering
		     graphviz-dot-mode
		     markdown-mode
		     plantuml-mode
		     projectile
		     csv-mode)
(rk-el-get-bundles kriyative/git-link restclient)

(defun rk-find-or-insert (expr insertion)
  (goto-char (point-min))
  (or (re-search-forward expr nil t)
      (progn
        (goto-char (point-max))
        (insert insertion))))

(defun rk-ensure-gpg-loopback-pinentry ()
  (let ((fname (expand-file-name "~/.gnupg/gpg-agent.conf")))
    (with-current-buffer (find-file-noselect fname)
      (dolist (cfg '("allow-emacs-pinentry"
                     "allow-loopback-pinentry"))
        (rk-find-or-insert (format "^[\s ]*%s[\s ]*$" cfg)
                           (format "\n%s" cfg)))
      (save-buffer))))

;; (rk-ensure-gpg-loopback-pinentry)

(use-package epa-file
  :config
  (setq epa-pinentry-mode 'loopback)
  (epa-file-enable)
  (rk-ensure-gpg-loopback-pinentry))

(defun rk-git-link-codecommit (hostname
                               dirname
                               filename
                               branch
                               commit
                               start
                               end)
  (let* ((matchp (string-match "\\([^\\.]*\\)\\.\\([^\\.]*\\)"
                               hostname))
         (region (when matchp
                   (match-string 2 hostname)))
         (domainname ".console.aws.amazon.com/codesuite/codecommit/repositories"))
    (format "https://%s/%s/browse/refs/heads/%s/--/%s"
	    (concat region domainname)
	    (file-name-nondirectory dirname)
	    (or branch commit)
	    (concat filename
                    (when start
                      (format "?lines=%s-%s"
                              start
                              (or end start)))))))

(use-package git-link
  :config
  ;; not needed any more, part of git-link now
  ;; (push '("git-codecommit" rk-git-link-codecommit) git-link-remote-alist)
  )

(use-package image
  :bind
  (:map image-map
        ("w" . image-transform-fit-to-width)
        ("h" . image-transform-fit-to-height)
        ("s" . image-transform-set-scale)))

(use-package window-numbering
  :config
  ;; (dotimes (i 10)
  ;;   (define-key ctlx-ctlj-map
  ;;     (prin1-to-string i)
  ;;     (intern (concat "select-window-" (prin1-to-string i)))))
  (window-numbering-mode)
  (window-numbering-update))

(use-package projectile
  :config
  (setq projectile-keymap-prefix (kbd "C-c C-p")))

(use-package csv-mode
  :config
  (setq csv-align-style 'auto))

(use-package graphviz-dot-mode
  :config
  (setq graphviz-dot-view-command "xdot %s"))

(use-package restclient)

(use-package plantuml-mode
  :config
  ;; why is the default to connect to plantuml.com?
  (setq plantuml-server-url nil
        plantuml-default-exec-mode 'jar))
