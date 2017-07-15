(require 'url)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

(defvar el-get-base-pkgs
  '(adaptive-wrap
    alert
    buffer-move
    edit-server
    emacs-w3m
    gh
    magit
    use-package))

(defvar el-get-extended-pkgs
  '( ;; clj-refactor
    bbdb
    csv-mode
    dictionary
    emacs-jabber
    emms
    emms-player-mpv
    ;; floobits
    ;; geiser
    gnuplot-mode
    guide-key
    graphviz-dot-mode
    ;; hydra
    ;; inflections
    jedi
    js2-mode
    lua-mode
    magit-gh-pulls
    markdown-mode
    ;; mode-line-stats
    ;; multiple-cursors
    org-gcal
    paredit
    plantuml-mode
    projectile
    ;; peg
    racket-mode
    rudel
    ;; symon
    ;; tree-mode
    ;; yasnippet
    slime
    xterm-color))

(defvar init-deps-hooks nil)

(defun load-extended-deps ()
  (el-get 'sync el-get-extended-pkgs)
  (el-get-bundle knu/elscreen)
  (el-get-bundle mu4e)
  (el-get-bundle csantosb/mu4e-multi)
  (el-get-bundle juergenhoetzel/password-mode)  
  (el-get-bundle agpchil/mu4e-maildirs-extension)
  (el-get-bundle iqbalansari/mu4e-alert)
  (el-get-bundle sshaw/git-link)
  (el-get-bundle bcbcarl/emacs-wttrin))

(defun init-deps ()
  (el-get 'sync el-get-base-pkgs)
  ;; fix cider at known version
  (el-get-bundle cider :checkout "v0.14.0")
  (el-get-bundle kriyative/emacs-fun :features (efun-base efun-cmds))
  (dolist (hook init-deps-hooks)
    (funcall hook)))
