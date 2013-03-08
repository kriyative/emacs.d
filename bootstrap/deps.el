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
(unless package-archive-contents
  (package-refresh-contents))

(dolist (pkg packages-deps)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(defvar non-packages-deps
  '(("emacs-w3m" :src "http://dl.dropbox.com/u/64576/share/sw/emacs-w3m.tar.gz")
    ("clojure-emacs-hacks" :src "git@github.com:kriyative/clojure-emacs-hacks.git")))

(defun download-and-install (src tmpdir fname)
  (let ((cmd (cond
	      ((string-match "^http" src)
	       (lambda ()
		 (eshell-command
		  (format "cd /tmp/%s; curl -O %s; cd ~/.emacs.d; tar xzf %s"
			  tmpdir
			  src
			  (concat "/tmp/" tmpdir "/" fname)))))
	      ((string-match "^git" src)
	       (lambda ()
		 (eshell-command
		  (format "cd ~/.emacs.d; git clone %s" src))))
	      (t (lambda ()
		   (message "Don't know how to download/install %s" src))))))
    (funcall cmd)))

(let ((tmpdir (make-temp-name "emacs-bootstrap-")))
  (eshell-command (format "mkdir -p /tmp/%s" tmpdir))
  (unwind-protect
      (dolist (dep non-packages-deps)
	(let* ((key (car dep))
	       (plist (cdr dep))
	       (src (getf plist :src))
	       (url (url-generic-parse-url src))
	       (fname (file-name-nondirectory (url-filename url)))
	       (dir (expand-file-name (concat "~/.emacs.d/" key))))
	  (unless (file-directory-p dir)
	    (message "downloading and unpacking %s ..." key)
	    (download-and-install src tmpdir fname))
	  (add-to-list 'load-path dir)))
    (eshell-command (format "rm -fr /tmp/%s" tmpdir))))
