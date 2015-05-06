(require 'cl)
(require 'url)

(defun emacs22-package-init ()
  (let ((elpa-package-el (expand-file-name "~/.emacs.d/elpa22/package.el")))
    (if (file-exists-p elpa-package-el)
        (load elpa-package-el)
        (let ((buffer (url-retrieve-synchronously
                       "http://tromey.com/elpa/package-install.el")))
          (save-excursion
            (set-buffer buffer)
            (goto-char (point-min))
            (re-search-forward "^$" nil 'move)
            (eval-region (point) (point-max))
            (kill-buffer (current-buffer)))
          (load elpa-package-el)))))

(defun emacs23-package-init ()
  (let ((emacs23-package-code (concat "http://repo.or.cz"
                                      "/w/emacs.git/blob_plain"
                                      "/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp"
                                      "/emacs-lisp/package.el"))
        (e23-package (expand-file-name "~/.emacs.d/elpa23/package.el")))
    (mkdir (file-name-directory e23-package) t)
    (unless (file-exists-p e23-package)
      (url-copy-file emacs23-package-code e23-package))
    (load e23-package)))

(cond
  ((emacs24p) (require 'package))
  ((emacs23p) (emacs23-package-init))
  ((emacs22p) (emacs22-package-init)))

(defconst additional-package-archives
  '((("kriyative" . "http://kriyative.github.io/elpa/packages/"))
    (("gnu" . "http://elpa.gnu.org/packages/"))
    (("marmalade" . "http://marmalade-repo.org/packages/") t)
    (("melpa" . "http://melpa.milkbox.net/packages/") t)
    ))

(when (boundp 'package-archives)
  (dolist (spec additional-package-archives)
    (apply 'add-to-list 'package-archives spec)))
(package-initialize)
(package-refresh-contents)

(defvar packages-deps
  `(adaptive-wrap
    ;; ac-cider-compliment
    ;; ac-nrepl
    adoc-mode
    buffer-move
    chicken-scheme
    cider
    clojure-emacs-hacks
    clojure-mode
    clojure-test-mode
    clojurescript-mode
    csv-mode
    dictionary
    edit-server
    emacs-w3m
    erlang
    gh
    google-contacts
    guide-key
    ,(if (emacs24-or-newer-p) 'js2-mode 'javascript-mode)
    jabber
    logito
    magit
    markup-faces
    miagi
    oauth2
    pcache
    slime-snapshot))

(defun bootstrap-emacs ()
  (interactive)
  (save-values (features)
    (let ((package-load-list (append (remove-if-not 'listp packages-deps) '(all))))
      (dolist (pkg packages-deps)
        (destructuring-bind (name version)
            (if (listp pkg)
                pkg
              (list pkg "any"))
          (unless (package-installed-p name)
            (message "installing %S" name)
            (condition-case c
                (package-install name)
              (error (message "Error installing %s %s" name c)))))))))
