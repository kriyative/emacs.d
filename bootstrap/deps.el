(require 'eshell)
(require 'url)

(defvar packages-deps
  '(adaptive-wrap
    adoc-mode
    buffer-move
    clojure-mode
    clojure-test-mode
    clojurescript-mode
    csv-mode
    gh
    jabber
    js2-mode
    logito
    magit
    magit-gh-pulls
    magithub
    markup-faces
    nrepl
    pcache
    slime
    slime-repl))

(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/")
	     t)
(package-initialize)

(dolist (pkg packages-deps)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(defvar non-packages-deps
  '(("emacs-w3m" :src "http://dl.dropbox.com/u/64576/share/sw/emacs-w3m.tar.gz")))

(let ((tmpdir (make-temp-name "emacs-bootstrap-")))
  (dolist (dep non-packages-deps)
    (let* ((key (car dep))
	   (plist (cdr dep))
	   (src (getf plist :src))
	   (url (url-generic-parse-url src))
	   (fname (file-name-nondirectory (url-filename url)))
	   (dir (expand-file-name (concat "~/.emacs.d/" key))))
      (unless (file-directory-p dir)
	(message "downloading and unpacking %s ..." key)
	(eshell-command (format "mkdir -p /tmp/%s" tmpdir))
	(eshell-command (format "cd /tmp/%s; curl -O %s" tmpdir src))
	(eshell-command (format "cd %s; tar xzf /tmp/%s/%s"
				(expand-file-name "~/.emacs.d/")
				tmpdir
				fname)))
      (add-to-list 'load-path dir)))
  (eshell-command (format "rm -fr /tmp/%s" tmpdir)))

