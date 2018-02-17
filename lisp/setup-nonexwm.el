(defun setup-elscreen ()
  (setq elscreen-display-tab nil))

(use-package elscreen
  :config (setup-elscreen)
  :bind (:map elscreen-map
              ("z" . elscreen-toggle)
              ("\C-z" . elscreen-toggle)))

(elscreen-start)
