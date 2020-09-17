(rk-el-get-bundles
 sshaw/git-link
 window-numbering
 graphviz-dot-mode
 markdown-mode
 plantuml-mode
 projectile
 csv-mode
 geiser
 restclient)

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

(use-package projectile
  :config
  (setq projectile-keymap-prefix (kbd "C-c C-p")))

(use-package csv-mode
  :config
  (setq csv-align-style 'auto))

(defun setup-geiser ()
  )

(use-package geiser
  :config
  (add-hook 'geiser-mode-hook 'setup-geiser))

(defun run-chez ()
  (interactive)
  (run-geiser 'chez))

(defun run-guile ()
  (interactive)
  (run-geiser 'guile))

(defun run-racket ()
  (interactive)
  (run-geiser 'racket))

(use-package graphviz-dot-mode
  :config
  (setq graphviz-dot-view-command "xdot %s"))

(use-package restclient)

(use-package plantuml-mode
  :config
  ;; why is the default to connect to plantuml.com?
  (setq plantuml-server-url nil
        plantuml-default-exec-mode 'jar))
