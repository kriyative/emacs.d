(my-el-get-bundles emms)

(defun rk-emms-browser ()
  (interactive)
  (delete-other-windows)
  (emms-playlist-mode-go))

(use-package emms
  :bind
  (:map user-commands-prefix-map
	("ee" . rk-emms-browser)
	("en" . emms-next)
	("ep" . emms-previous)
	("SPC" . emms-pause))
  :config
  (add-to-list 'emms-player-base-format-list "opus")
  (emms-all)
  (emms-default-players)
  (setq emms-source-file-default-directory "~/Music/"
	emms-player-mplayer-parameters '("-slave" "-quiet" "-really-quiet" "-vo" "null"))
  ;; (require 'emms-player-mpd)
  ;; (add-to-list 'emms-player-list 'emms-player-mpd)
  ;; (add-to-list 'emms-info-functions 'emms-info-mpd)
  ;; (setq emms-player-mpd-server-name "localhost"
  ;;       emms-player-mpd-server-port "6600")
  (setq emms-playlist-default-major-mode 'emms-playlist-mode)
  (add-hook 'emms-info-functions 'emms-info-track-description))
