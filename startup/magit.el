(defun rk--magit-setup-hook ()
  (local-unset-key [C-tab])
  (define-key magit-mode-map [C-tab] nil))

(defun rk-magit-show-refs-sort-by-committerdate (&optional transient)
  (interactive)
  (setq-local magit-buffer-arguments '("--sort=-committerdate"))
  (magit-show-refs transient))

(use-package magit
  :straight t
  :config
  (when (facep 'magit-item-highlight)
    (set-face-attribute 'magit-item-highlight nil
                        :background "lightgrey"
                        :foreground "black"))
  (when (facep 'magit-tag)
    (set-face-attribute 'magit-tag nil :foreground "black"))
  (setq magit-last-seen-setup-instructions "1.4.0")
  (add-hook 'magit-mode-hook 'rk--magit-setup-hook)
  :bind
  (("H-v" . magit-status)
   :map magit-status-mode-map
   ("y" . magit-show-refs)
   ("M-y" . rk-magit-show-refs-sort-by-committerdate)
   :map magit-refs-mode-map
   ("y" . magit-show-refs)
   ("M-y" . rk-magit-show-refs-sort-by-committerdate))
  :bind
  (:map user-commands-prefix-map
        ("v" . magit-status)))

(use-package magit-todos
  :straight t
  :config
  (add-to-list 'magit-todos-keywords-list "REVIEW"))

(use-package git-code-review
  :straight (git-code-review :type git
                             :host github
                             :repo "kriyative/git-code-review")
  :config
  (add-hook 'clojure-mode-hook 'gcr-mode)
  (add-hook 'emacs-lisp-mode-hook 'gcr-mode)
  (add-hook 'common-lisp-mode-hook 'gcr-mode))
