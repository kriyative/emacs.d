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
        org-passwords
	use-package))

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
                graphviz-dot-mode
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
                plantuml-mode
                projectile
                ;; peg
                rudel
                ;; symon
                ;; tree-mode
                ;; yasnippet
                slime
                )))

(el-get-bundle magit/git-modes :checkout "1.0.0")
(el-get-bundle magit/magit :checkout "1.4.2")
;; (el-get-bundle cider :checkout "v0.8.2")
(el-get-bundle cider :checkout "v0.14.0")
(el-get-bundle juergenhoetzel/password-mode)

;; (el-get-bundle expez/edn.el)

(el-get-bundle kriyative/emacs-fun :features (efun-base efun-cmds))
(el-get 'sync el-get-full-pkgs)
(el-get-bundle agpchil/mu4e-maildirs-extension)
(el-get-bundle iqbalansari/mu4e-alert)
