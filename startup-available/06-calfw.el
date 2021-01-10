(rk-el-get-bundles
 kiwanami/emacs-calfw
 myuhe/calfw-gcal.el)

(use-package calfw
  :config
  ;; Unicode characters
  (setq cfw:fchar-junction ?╋
        cfw:fchar-vertical-line ?┃
        cfw:fchar-horizontal-line ?━
        cfw:fchar-left-junction ?┣
        cfw:fchar-right-junction ?┫
        cfw:fchar-top-junction ?┯
        cfw:fchar-top-left-corner ?┏
        cfw:fchar-top-right-corner ?┓))

(use-package calfw-ical)
(use-package calfw-cal
  :bind (:map user-commands-prefix-map
              ("c" . cfw:open-diary-calendar))
  )
(use-package calfw-gcal)
