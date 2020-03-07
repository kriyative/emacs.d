(rk-el-get-bundles
 sshaw/git-link
 window-numbering)

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

(use-package git-link)

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
