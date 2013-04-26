(defun setup-lisp-indent-function (&optional indent-function)
  (let ((indent-function (or indent-function 'lisp-indent-function))
        (lisp-indent-alist '((awhen . 1)
                             (when-let . 1)
                             (aif . if)
                             (if-let . if)
                             (awhile . 1)
                             (while-let . 1)
                             (bind . 1)
                             (callback . lambda)
                             (c-fficall . with-slots)
                             (with-cwd . 1)
                             (save-values . 1))))
    (dolist (x lisp-indent-alist)
      (put (car x)
           indent-function
           (if (numberp (cdr x))
               (cdr x)
             (get (cdr x) indent-function))))))

(defun set-common-lisp-block-comment-syntax ()
  (modify-syntax-entry ?# "<1" font-lock-syntax-table)
  (modify-syntax-entry ?| ">2" font-lock-syntax-table)
  (modify-syntax-entry ?| "<3" font-lock-syntax-table)
  (modify-syntax-entry ?# ">4" font-lock-syntax-table))

(defun my-common-lisp-mode-hook ()
  (font-lock-mode)
  (font-lock-add-keywords 'lisp-mode
			  '(("defclass\*" . font-lock-keyword-face)))
  (modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)
  (set (make-local-variable 'lisp-indent-function) 'common-lisp-indent-function)
  (setup-lisp-indent-function 'common-lisp-indent-function)
  (setq indent-tabs-mode nil))

(defun lispdoc ()
  "Searches lispdoc.com for SYMBOL, which is by default the symbol
currently under the curser"
  (interactive)
  (let* ((word-at-point (word-at-point))
	 (symbol-at-point (symbol-at-point))
	 (default (symbol-name symbol-at-point))
	 (inp (read-from-minibuffer
	       (if (or word-at-point symbol-at-point)
		   (concat "Symbol (default " default "): ")
		 "Symbol (no default): "))))
    (if (and (string= inp "")
	     (not word-at-point)
	     (not symbol-at-point))
	(message "you didn't enter a symbol!")
      (let ((search-type (read-from-minibuffer
			  "full-text (f) or basic (b) search (default b)? ")))
	(browse-url (concat "http://lispdoc.com?q="
			    (if (string= inp "")
				default
			      inp)
			    "&search;="
			    (if (string-equal search-type "f")
				"full+text+search"
			      "basic+search")))))))

(defun my-slime-list-connections ()
  (interactive)
  (slime-list-connections)
  (pop-to-buffer "*SLIME Connections*"))

(defun my-slime-mode-hook ()
  (setq common-lisp-hyperspec-root
	"file:///opt/cl-doc/HyperSpec/"
	;; "http://www.lispworks.com/reference/HyperSpec/"
	browse-url-browser-function 'w3m-browse-url-other-window)
  ;; (set-face-attribute 'slime-highlight-edits-face nil :background "grey")
  (define-key slime-mode-map "\M-\C-x" 'slime-compile-defun)
  (define-key slime-mode-map "\C-c\C-xc" 'my-slime-list-connections))

(defun slime-mode-init ()
  (slime-setup '(slime-repl))
  (setq slime-protocol-version 'ignore)
  (add-hook 'slime-mode-hook 'my-slime-mode-hook))

(eval-after-load 'slime
  '(slime-mode-init))

(defun sbcl ()
  (interactive)
  (if-let (sbcl-path (locate-path "sbcl" exec-path))
      (let ((slime-lisp-implementations `((sbcl (,sbcl-path)))))
        (setenv "SBCL_HOME" (file-name-directory sbcl-path))
        (slime))
    (message "The sbcl application could not be found")))

(defun ccl ()
  (interactive)
  (if-let (ccl-path (locate-path "ccl64" exec-path))
      (let ((slime-lisp-implementations `((ccl (,ccl-path)))))
        (slime))
    (message "The ccl application could not be found")))

(defun clisp ()
  (interactive)
  (if-let (clisp-path (locate-path "clisp" exec-path))
      (let ((slime-lisp-implementations `((clisp (,clisp-path " -K full")))))
        (slime))
    (message "The clisp application could not be found")))

(defun ecl ()
  (interactive)
  (if-let (ecl-path (locate-path "ecl.sh" exec-path))
      (let ((slime-lisp-implementations `((ecl (,ecl-path)))))
        (slime))
    (message "The ecl application could not be found")))

;;;;;;;;;;;;;;;; emacs lisp ;;;;;;;;;;;;;;;;

(defun my-emacs-lisp-mode-hook ()
  ;; (local-set-key "	" 'lisp-complete-symbol)
  (outline-minor-mode 1)
  (setq outline-regexp "^[(;]"
	indent-tabs-mode nil)
  (setup-lisp-indent-function)
  (local-set-key "\M-." 'find-function)
  (font-lock-mode 1)
  (eldoc-mode 1))

(defun lisp-mode-init ()
  (add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
  (add-hook 'lisp-interaction-mode-hook 'my-emacs-lisp-mode-hook)
  (add-hook 'lisp-mode-hook 'my-common-lisp-mode-hook)
  (add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode)))

(eval-after-load 'lisp-mode
  '(lisp-mode-init))

;;;;;;;;;;;;;;;; scheme ;;;;;;;;;;;;;;;;

(defun scheme-mode-hook ()
  (make-local-variable 'eldoc-documentation-function)
  (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
  (eldoc-mode 1)
  (setq lisp-indent-function 'scheme-smart-indent-function))

(defun scheme-mode-init ()
  (require 'quack)
  (require 'scheme-complete)
  (add-to-list 'exec-path (expand-file-name "~/Applications/PLT Scheme v4.2.1/bin/"))
  (define-key scheme-mode-map "\e\t" 'scheme-smart-complete)
  (add-hook 'scheme-mode-hook 'scheme-mode-hook))

(eval-after-load 'scheme-mode
  '(scheme-mode-init))

(defun mzscheme ()
  (interactive)
  (let ((inferior-lisp-program
         (expand-file-name "~/Applications/PLT\\ Scheme\\ v4.2.1/bin/mzscheme")))
    (run-lisp inferior-lisp-program)))

(eval-after-load 'chicken-slime
  '(require 'cmuscheme))

(add-to-list 'load-path "/usr/local/lib/chicken/6/")
(autoload 'chicken-slime "chicken-slime" "SWANK backend for Chicken" t)

(defun chicken-doc (&optional obtain-function)
  (interactive)
  (let ((func (funcall (or obtain-function 'current-word))))
    (when func
      (process-send-string (scheme-proc)
                           (format "(require-library chicken-doc) ,doc %S\n" func))
      (save-selected-window
        (select-window (display-buffer (get-buffer scheme-buffer) t))
        (goto-char (point-max))))))

;;;;;;;;;;;;;;;; clojure ;;;;;;;;;;;;;;;;

(eval-after-load 'clojure-mode
  '(progn
    (require 'clojure-mode-ext)
    (require 'clojure-mode-slime)
    (require 'clojuredocs)))

(eval-after-load 'clojure-test-mode
  '(require 'clojure-test-mode-slime))

(defun setup-clojurescript-mode ()
  (define-clojure-indent
    (this-as 1)))

(eval-after-load 'clojurescript-mode
  '(progn
     (require 'clojurescript-mode-ext)
     (setup-clojurescript-mode)))
