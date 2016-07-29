(require 'url)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

(defvar el-get-minimal-pkgs nil)
(setq el-get-minimal-pkgs
      '(adaptive-wrap
        alert
        buffer-move
        edit-server
        emacs-w3m
        magit
        org-passwords))

(defvar el-get-full-pkgs nil)
(setq el-get-full-pkgs
      (append el-get-minimal-pkgs
              '(;; clj-refactor
                bbdb
                csv-mode
                dictionary
                ;; emacs-jabber
                emms
                emms-player-mpv
                ;; floobits
                ;; geiser
                guide-key
                ;; hydra
                ;; inflections
                jedi
                js2-mode
                lua-mode
                markdown-mode
                ;; mode-line-stats
                ;; multiple-cursors
                mu4e
                mu4e-multi
                paredit
                projectile
                ;; peg
                ;; rudel
                ;; symon
                ;; tree-mode
                ;; yasnippet
                slime
		use-package
                )))

(el-get-bundle cider :checkout "v0.8.2")
(el-get-bundle juergenhoetzel/password-mode)
(el-get-bundle kriyative/emacs-fun :features (efun-base efun-cmds))
;; (el-get-bundle expez/edn.el)

(el-get 'sync el-get-full-pkgs)
