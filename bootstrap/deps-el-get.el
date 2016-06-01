(require 'url)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

(defvar el-get-pkgs nil)
(setq el-get-pkgs
      '(adaptive-wrap
        alert
        buffer-move
        csv-mode
        dictionary
        edit-server
        ;; emacs-jabber
        emacs-w3m
        ;; emms
        ;; geiser
        guide-key
        jedi
        js2-mode
        lua-mode
        magit
        mode-line-stats
        org-passwords
        ;; rudel
        symon))

;; (el-get-bundle clojure-emacs/ac-cider :checkout "0.2.1")
(el-get-bundle cider :checkout "v0.8.2")
(el-get-bundle juergenhoetzel/password-mode)
(el-get-bundle kriyative/emacs-fun)

(el-get 'sync el-get-pkgs)
