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
    (("marmalade" . "http://marmalade-repo.org/packages/") t)
    (("melpa" . "http://melpa.milkbox.net/packages/") t)))

(when (boundp 'package-archives)
  (dolist (spec additional-package-archives)
    (apply 'add-to-list 'package-archives spec)))
(package-initialize)
(package-refresh-contents)

(defvar packages-deps
  `(adaptive-wrap
    adoc-mode
    buffer-move
    chicken-scheme
    clojure-emacs-hacks
    clojure-mode
    clojure-test-mode
    clojurescript-mode
    csv-mode
    dictionary
    emacs-w3m
    erlang
    gh
    google-contacts
    guide-key
    jabber
    ,(if (emacs24-or-newer-p) 'js2-mode 'javascript-mode)
    logito
    magit
    magit-gh-pulls
    ;; magithub
    markup-faces
    miagi
    nrepl
    oauth2
    pcache
    slime-snapshot))

(save-values (features)
  (dolist (pkg packages-deps)
    (unless (package-installed-p pkg)
      (condition-case nil
	  (package-install pkg)
	(error (message "Error installing %s" pkg))))))
