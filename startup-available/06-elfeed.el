(rk-el-get-bundles elfeed)

(defun rk--show-elfeed-buffer (buf)
  (display-buffer-same-window buf nil))

(use-package elfeed
  :config
  ;; elfeed-feeds are set in ~/.personal.el
  (setq elfeed-show-entry-switch 'rk--show-elfeed-buffer)
  :bind
  (:map elfeed-search-mode-map
        ("U" . elfeed-update)))
