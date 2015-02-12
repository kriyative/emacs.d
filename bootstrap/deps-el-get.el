(require 'url)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

(defvar pkgs
  '(adaptive-wrap
    alert
    buffer-move
    csv-mode
    dictionary
    edit-server
    emacs-jabber
    emacs-w3m
    guide-key
    js2-mode
    lua-mode
    magit))

(el-get 'sync pkgs)

(el-get-bundle clojure-emacs/ac-cider :checkout "0.2.1")
(el-get-bundle cider :checkout "v0.8.2")