;; snarfed from:
;; https://github.com/magit/transient/issues/17#issuecomment-493030702
(with-eval-after-load 'transient
  (setq
   transient--buffer-name "*transient*"
   ;; transient-detect-key-conflicts t
   ;; transient-highlight-mismatched-keys t
   ;; transient--debug t
   transient-enable-popup-navigation t
   transient-mode-line-format mode-line-format
   transient-display-buffer-action '(display-buffer-below-selected))

  (let ((map transient-base-map))
    (define-key map (kbd "C-g") 'transient-quit-all)
    (define-key map (kbd "C-q") 'transient-quit-one)
    (define-key map (kbd "DEL") 'transient-quit-one))

  (let ((map transient-map))
    (define-key map (kbd "C-h") nil)
    (define-key map (kbd "C-p") 'transient-backward-button)
    (define-key map (kbd "C-n") 'transient-forward-button))

  (let ((map transient-popup-navigation-map))
    (define-key map (kbd "<tab>") 'transient-forward-button)
    (define-key map (kbd "<backtab>") 'transient-backward-button ))

  (transient-suffix-put 'transient-common-commands
                        "C-g" :command 'transient-quit-all)
  (transient-suffix-put 'transient-common-commands
                        "C-q" :command 'transient-quit-one)

  (defun al/transient-fix-window ()
    "Return `transient--window' to a 'normal' state."
    (set-window-dedicated-p transient--window nil)
    (set-window-parameter transient--window 'no-other-window nil)
    (with-selected-window transient--window
      (setq
       window-size-fixed nil
       cursor-in-non-selected-windows t
       cursor-type (default-value 'cursor-type)
       mode-line-buffer-identification
       (list ""
             (symbol-name (oref transient--prefix command))
             " " (default-value 'mode-line-buffer-identification)))))

  (define-derived-mode al/transient-mode special-mode "al/transient"
    (setq buffer-read-only t)
    (al/transient-fix-window))

  (defun al/transient-push-keymap (map)
    (with-demoted-errors "al/transient-push-keymap: %S"
      (internal-push-keymap (symbol-value map) 'al/transient-mode-map)))

  (defun al/transient-pop-keymap (map)
    (with-demoted-errors "al/transient-pop-keymap: %S"
      (internal-pop-keymap (symbol-value map) 'al/transient-mode-map)))

  (defun al/transient-fix-show (&rest _)
    (transient--debug 'al/transient-fix-show)
    (al/transient-fix-window)
    (select-window transient--window))

  (defun al/transient-fix-init (&rest _)
    (transient--debug 'al/transient-fix-init)
    (with-current-buffer transient--buffer-name
      (al/transient-mode)))

  (defun al/transient-fix-pre/post-command (fun &rest args)
    (transient--debug 'al/transient-fix-pre/post-command)
    ;; Do anything only for transient commands.
    (when (or (get this-command 'transient--prefix)
              (string-match-p "\\`transient"
                              (symbol-name this-command))
              (and transient--transient-map
                   (string= (buffer-name) transient--buffer-name)
                   (lookup-key transient--transient-map
                               (this-single-command-raw-keys))))
      (apply fun args)))

  (defun al/transient-fix-delete-window (fun &rest args)
    (unless (eq transient--exitp 'suspend)
      (apply fun args)))

  (advice-add 'transient--minibuffer-setup :override #'ignore)
  (advice-add 'transient--minibuffer-exit :override #'ignore)
  (advice-add 'transient--push-keymap :override #'al/transient-push-keymap)
  (advice-add 'transient--pop-keymap :override #'al/transient-pop-keymap)
  (advice-add 'transient--pre-command :around #'al/transient-fix-pre/post-command)
  (advice-add 'transient--post-command :around #'al/transient-fix-pre/post-command)
  (advice-add 'transient--show :after #'al/transient-fix-show)
  (advice-add 'transient--init-transient :after #'al/transient-fix-init)
  (advice-add 'transient--delete-window :around #'al/transient-fix-delete-window))

