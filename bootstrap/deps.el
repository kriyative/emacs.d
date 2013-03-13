(require 'eshell)
(require 'url)

(defun emacs22-package-init ()
  (let ((elpa-package-el (expand-file-name "~/.emacs.d/elpa/package.el")))
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

(when (boundp 'package-archives)
  (add-to-list 'package-archives
	       '("marmalade" . "http://marmalade-repo.org/packages/")
	       t))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(defvar packages-deps
  `(adaptive-wrap
    adoc-mode
    buffer-move
    clojure-mode
    clojure-test-mode
    clojurescript-mode
    csv-mode
    gh
    jabber
    ,(if (emacs24-or-newer-p) 'js2-mode 'javascript-mode)
    logito
    magit
    magit-gh-pulls
    magithub
    markup-faces
    nrepl
    pcache
    slime
    slime-repl))

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
