(defun rk-emms-browser ()
  (interactive)
  (delete-other-windows)
  (emms-playlist-mode-go))

(defun rk-emms-mode-line-playlist-current-name ()
  "Format the currently playing song."
  (format emms-mode-line-format
          (file-name-nondirectory
           (emms-track-name
            (emms-playlist-current-selected-track)))))

(defun rk-emms-show ()
  (interactive)
  (let ((emms-show-format (if emms-player-paused-p
                              "EMMS paused %s"
                            "EMMS playing %s")))
    (emms-show)))

(defun rk--emms-echo-track-info ()
  (rk-emms-show))

(use-package emms
  :straight t
  :bind
  (:map user-commands-prefix-map
        ("eb" . rk-emms-browser)
        ("ee" . rk-emms-browser)
        ("en" . emms-next)
        ("ep" . emms-previous)
        (" " . emms-pause)
        ("es" . rk-emms-show)
        ("ed" . emms-play-directory)
        ("et" . emms-play-directory-tree)
        ("ef" . emms-play-file))
  :bind
  (:map dired-mode-map
        ("\C-ce" . emms-play-dired))
  :config
  (add-to-list 'emms-player-base-format-list "opus")
  (emms-all)
  (emms-default-players)
  (setq emms-player-list (list emms-player-mpv)
        emms-player-mpv-parameters (cons "--no-video" emms-player-mpv-parameters)
        emms-source-file-default-directory "~/Music/"
        emms-player-mplayer-parameters '("-slave"
                                         "-quiet"
                                         "-really-quiet"
                                         "-vo" "null"))
  ;; (require 'emms-player-mpd)
  ;; (add-to-list 'emms-player-list 'emms-player-mpd)
  ;; (add-to-list 'emms-info-functions 'emms-info-mpd)
  ;; (setq emms-player-mpd-server-name "localhost"
  ;;       emms-player-mpd-server-port "6600")
  (setq emms-playlist-default-major-mode 'emms-playlist-mode)
  (add-hook 'emms-info-functions 'emms-info-track-description)
  (add-hook 'emms-player-started-hook 'rk--emms-echo-track-info)
  (add-hook 'emms-player-paused-hook 'rk--emms-echo-track-info)
  (add-hook 'emms-playlist-selection-changed-hook 'rk--emms-echo-track-info)
  (emms-mode-line -1)
  (emms-playing-time -1))
