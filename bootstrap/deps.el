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
    ("clojure-emacs-hacks" :src "https://github.com/kriyative/clojure-emacs-hacks.git")))

(defmacro with-cwd (dir &rest body)
  (let ((cwd% (gensym)))
    `(let ((,cwd% default-directory))
       (unwind-protect
           (progn
             (cd-absolute ,dir)
             ,@body)
         (cd-absolute ,cwd%)))))

(defvar init-dir "~/.emacs.d")

(defun test-string-match (item x) (string-match x item))

(defvar unpackers-alist
  `(("\\(tgz\\|tar\.gz\\)$"
     ,(lambda (filename)
        (call-process "tar" nil nil nil "xzf" filename)))
    ("\\(tbz\\|tar\.bz\\|tar\.bz2\\)$"
     ,(lambda (filename)
        (call-process "tar" nil nil nil "xjf" filename)))
    ("tar$"
     ,(lambda (filename)
        (call-process "tar" nil nil nil "xf" filename)))
    ("zip$"
     ,(lambda (filename)
        (call-process "unzip" nil nil nil filename)))))

(defun find-unpacker (filename)
  (when-let (unpacker (assoc* filename unpackers-alist :test 'test-string-match))
    (cadr unpacker)))

;; (find-unpacker "emacs-w3m.tar.gz")
;; (find-unpacker "master.zip")

(defun unpack (filename)
  (when-let (unpacker (find-unpacker filename))
    (funcall unpacker filename)))

(defun download-and-install-http (src tmpdir fname)
  (with-cwd tmpdir
    (call-process "curl" nil nil nil "-L" "-O" src))
  (with-cwd init-dir
    (unpack (concat (file-name-as-directory tmpdir) fname))))

(defun download-and-install-git (src tmpdir fname)
  (with-cwd init-dir
    (call-process "git" nil nil nil "clone" src)))

(defvar downloaders-alist
  '(("github\.com" download-and-install-git)
    ("^http" download-and-install-http)))

(defun find-downloader (src)
  (when-let (downloader (assoc* src downloaders-alist :test 'test-string-match))
    (cadr downloader)))

(defun download-and-install (src &rest args)
  (if-let (downloader (find-downloader src))
      (apply downloader src args)
    (message "Don't know how to download/install %s" src)))

(let* ((tmpname (make-temp-name "emacs-bootstrap-"))
       (tmpdir (format "/tmp/%s" tmpname)))
  (unwind-protect
      (progn
        (make-directory tmpdir t)
        (add-to-list 'exec-path "/usr/local/bin")
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
            (add-to-list 'load-path dir))))
    (delete-directory tmpdir t)))
