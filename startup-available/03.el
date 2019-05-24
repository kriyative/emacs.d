(my-el-get-bundles
 sshaw/git-link
 window-numbering)

(use-package epa-file
  :config
  (epa-file-enable))

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
