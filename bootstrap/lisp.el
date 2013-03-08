(defun setup-common-lisp-indent-function ()
  (set (make-local-variable 'lisp-indent-function) 'common-lisp-indent-function)
  (dolist (x '((awhen . 1) (when-let . 1) (aif . if) (if-let . 1)
               (awhile . 1) (while-let . 1) (bind . 1)
               (callback . lambda) (c-fficall . with-slots)))
    (put (car x) 'common-lisp-indent-function
         (if (numberp (cdr x))
             (cdr x)
           (get (cdr x) 'common-lisp-indent-function)))))

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
  (setup-common-lisp-indent-function)
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
  (require 'slime-repl)
  (slime-setup '(slime-repl))
  (setq slime-protocol-version 'ignore)
  ;; (setq slime-use-autodoc-mode nil)
  (add-hook 'slime-mode-hook 'my-slime-mode-hook)
  ;; (add-hook 'slime-connected-hook 'slime-connected-hook)
  )

(eval-after-load "slime"
  '(slime-mode-init))

(defun sbcl ()
  (interactive)
  (let* ((sbcl-path "/opt/sbcl/bin/sbcl")
         (slime-lisp-implementations `((sbcl '(,sbcl-path)))))
    (setenv "SBCL_HOME" sbcl-path)
    (slime)))

(defun ccl ()
  (interactive)
  (let* ((ccl-path "/opt/src/ccl")
         (slime-lisp-implementations `((ccl  (concat ccl-path "/scripts/ccl64")))))
    (setenv "CCL_DEFAULT_DIRECTORY" ccl-path)
    (slime)))

(defun clisp ()
  (interactive)
  (let ((slime-lisp-implementations
	 `((clisp ,(list (locate-path "clisp" exec-path) " -K full")))))
    (slime)))

(defun ecl ()
  (interactive)
  (let ((slime-lisp-implementations
         `((ecl ,(list (expand-file-name "~/bin/ecl.sh")))))
	(slime-net-coding-system 'utf-8-unix))
    (slime)))

(defun ecl-iphone ()
  (interactive)
  (let ((slime-lisp-implementations
	 `((ecl ,(list "/opt/iphone/simulator/bin/ecl")))))
    (setenv "DYLD_LIBRARY_PATH" "/opt/iphone/simulator/lib")
    (slime)))

;;;;;;;;;;;;;;;; emacs lisp ;;;;;;;;;;;;;;;;

(defun my-emacs-lisp-mode-hook ()
  ;; (local-set-key "	" 'lisp-complete-symbol)
  (outline-minor-mode 1)
  (setq outline-regexp "^[(;]"
	indent-tabs-mode nil)
  (setup-common-lisp-indent-function)
  (local-set-key "\M-." 'find-function)
  (font-lock-mode 1)
  (eldoc-mode 1))

(defun lisp-mode-init ()
  (add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
  (add-hook 'lisp-interaction-mode-hook 'my-emacs-lisp-mode-hook)
  (add-hook 'lisp-mode-hook 'my-common-lisp-mode-hook)
  (add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode)))

(eval-after-load 'emacs-lisp-mode
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

;;;;;;;;;;;;;;;; clojure ;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "~/.emacs.d/clojure-emacs-hacks/"))

(eval-after-load 'clojure-mode
  '(progn
     (require 'clojure-mode-ext)
     (require 'clojure-mode-slime)
     (require 'clojuredocs)))
