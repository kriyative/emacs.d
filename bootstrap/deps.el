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
    (unless (file-exists-p e23-package)
      (url-copy-file emacs23-package-code e23-package))
    (load e23-package)))

(cond
  ((emacs24p) (require 'package))
  ((emacs23p) (emacs23-package-init))
  ((emacs22p) (emacs22-package-init)))

(defconst additional-package-archives
  '((("kriyative" . "http://kriyative.github.com/elpa/packages/"))
    (("marmalade" . "http://marmalade-repo.org/packages/") t)))

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
    clojure-mode
    clojure-test-mode
    clojurescript-mode
    clojure-emacs-hacks
    csv-mode
    gh
    jabber
    ,(if (emacs24-or-newer-p) 'js2-mode 'javascript-mode)
    logito
    magit
    magit-gh-pulls
    magithub
    markup-faces
    miagi
    nrepl
    pcache
    slime-snapshot
    emacs-w3m))

(save-values (features)
  (dolist (pkg packages-deps)
    (unless (package-installed-p pkg)
      (package-install pkg))))
