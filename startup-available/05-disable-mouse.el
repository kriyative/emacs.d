(rk-el-get-bundles
 (disable-mouse :url "https://github.com/purcell/disable-mouse.git"
                :features disable-mouse))

(use-package disable-mouse
  :diminish disable-mouse-global-mode
  :delight disable-mouse-global-mode
  :config
  (global-disable-mouse-mode))
