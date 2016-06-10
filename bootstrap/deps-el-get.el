(require 'url)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

(defvar el-get-minimal-pkgs '(adaptive-wrap
                              alert
                              buffer-move
                              edit-server
                              emacs-w3m
                              magit
                              org-passwords))
(defvar el-get-full-pkgs (append el-get-minimal-pkgs
                                 '(csv-mode
                                   dictionary
                                   ;; emacs-jabber
                                   emms
				   emms-player-mpv
                                   ;; geiser
                                   guide-key
                                   jedi
                                   js2-mode
                                   lua-mode
                                   mode-line-stats
                                   ;; rudel
                                   symon)))

;; (el-get-bundle clojure-emacs/ac-cider :checkout "0.2.1")
(el-get-bundle cider :checkout "v0.8.2")
(el-get-bundle juergenhoetzel/password-mode)
(el-get-bundle kriyative/emacs-fun)

(el-get 'sync el-get-minimal-pkgs)
