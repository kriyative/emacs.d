;;;;;;;;;;;;;;;; lisp ;;;;;;;;;;;;;;;;

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
  (define-key slime-mode-map "\C-c\C-xc" 'my-slime-list-connections)
  (unless (boundp 'last-command-char)
    (defvar last-command-char nil)))

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

(defun clojure-mode-hook ()
  (auto-revert-mode 1)
  (outline-minor-mode 1))

(eval-after-load 'clojure-mode
  '(progn
     (add-hook 'clojure-mode-hook 'clojure-mode-hook)
     (add-to-list 'auto-mode-alist '("\\.cljc\\'" . clojure-mode))))

(eval-after-load 'cider-repl
  '(cider-repl-add-shortcut "sayoonara" 'cider-quit))

(defun cider-mode-hook ()
  (cider-turn-on-eldoc-mode)
  (outline-minor-mode)
  (define-key cider-mode-map "\C-c\C-k" 'cider-load-buffer-ext)
  (setq cider-completion-use-context nil))

;; (remove-hook 'cider-mode-hook 'cider-mode-hook)

(defun cider-setup ()
  (add-hook 'cider-mode-hook 'cider-mode-hook)
  (setq cider-lein-parameters "trampoline repl :headless"))

(eval-after-load 'cider
  `(cider-setup))

(defun cider-remove-current-ns (&optional buffer)
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (cider-tooling-eval
     (format "(remove-ns '%s)" (cider-current-ns))
     (cider-interactive-eval-handler (current-buffer)))))

(defun cider-load-buffer-ext (&optional arg)
  (interactive "p")
  (if (< 1 arg)
      (progn
        (message "Removing namespace: %s" (cider-current-ns))
        (cider-remove-current-ns)
        (cider-load-buffer))
    (cider-load-buffer)))

;;;;;;;;;;;;;;;; c mode ;;;;;;;;;;;;;;;;

(defun c-mode-hook ()
  (setq tab-width 8
	c-basic-offset 4
	indent-tabs-mode nil)
  (font-lock-mode 1))

(eval-after-load 'cc-mode
  '(add-hook 'c-mode-hook 'c-mode-hook))

(defun objc-mode-setup-hook ()
  (setq tab-width 8
	c-basic-offset 4
	indent-tabs-mode nil
	truncate-lines t))

(eval-after-load 'cc-mode
  '(add-hook 'objc-mode-hook 'objc-mode-setup-hook))

;;;;;;;;;;;;;;;; java mode ;;;;;;;;;;;;;;;;

(defun java-beginning-of-method ()
  (when (re-search-backward (concat "^\\(  \\|\t\\)"
				    "\\(synchronized"
				    "\\|public"
				    "\\|protected"
				    "\\|private\\)")
			    nil
			    'move
			    1)
    (goto-char (1- (match-end 0)))))

(defun java-end-of-method ()
  (let ((result (re-search-forward "^  }" nil t)))
    (end-of-line)
    (and result t)))

(defun java-mode-hook ()
  (setq c-basic-offset 4
	tab-width 4
	c-set-style "linux"
	indent-tabs-mode nil)
  (when (boundp 'outline-regexp)
    (outline-minor-mode 1)
    (setq outline-regexp (concat "^\\(  [  ]*\\|\\)"
                                 "\\(synchronized"
                                 "\\|public"
                                 "\\|protected"
                                 "\\|private"
                                 ;; "\\|//"
                                 "\\)")))
  (font-lock-mode 1))

(defun java-mode-init ()
  ;; (setenv "JAVA_HOME" (opt-path "jdk"))
  (add-hook 'java-mode-hook 'java-mode-hook)
  (require 'compile)
  (setq compilation-error-regexp-alist
        (append
         (list
          ;; works for jikes
          '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):\\([0-9]+\\):[0-9]+:[0-9]+:"
            1 2 3)
          ;; works for javac
          '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):" 1 2)

          ;; clojure.test
          '("^FAIL in (.*) (\\([^:]*\\):\\([0-9]+\\)" 1 2)
          '("^ERROR in (.*) (\\([^:]*\\):\\([0-9]+\\)" 1 2))
         compilation-error-regexp-alist)))

;; (setq auto-mode-alist (cons '("\\.cs\\'" . java-mode) auto-mode-alist))

(eval-after-load 'cc-mode
  '(java-mode-init))

;;;;;;;;;;;;;;;; c-sharp mode ;;;;;;;;;;;;;;;;

;; override a buggy defadvice in csharp-mode.el
;;
(eval-after-load 'csharp-mode
  '(defadvice revert-buffer (around
                             csharp-advise-revert-buffer
                             activate compile)
     (let ((is-flymake-enabled
            (and (boundp 'flymake-is-running)
                 flymake-is-running)))
       ;; disable
       (if is-flymake-enabled
           (flymake-mode-off))

       ;; revert
       ad-do-it

       ;; enable
       (if is-flymake-enabled
           (flymake-mode-on)))))

;;;;;;;;;;;;;;;; js2-mode ;;;;;;;;;;;;;;;;

(defun my-js2-mode-hook ()
  (setq indent-tabs-mode nil))

(defun setup-js2-mode ()
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook 'my-js2-mode-hook))

(eval-after-load 'js2-mode
  '(setup-js2-mode))

;;;;;;;;;;;;;;;; python ;;;;;;;;;;;;;;;;

(defun setup-python ()
  (setq python-remove-cwd-from-path nil)
  (setq jedi:setup-keys t)
  (require 'jedi)
  (add-hook 'python-mode-hook 'jedi:setup))

(eval-after-load 'python
  '(setup-python))

;;;;;;;;;;;;;;;; Bigloo BEE ;;;;;;;;;;;;;;;;

(add-to-list 'load-path "/usr/local/share/emacs/24.5/site-lisp/bigloo")
(add-to-list 'exec-path "/app/bigloo/bin")
(autoload 'bdb "bdb" "bdb mode" t)
(autoload 'bee-mode "bee-mode" "bee mode" t)

(defun setup-bee ()
  (setq auto-mode-alist
        (append '(("\\.scm$" . bee-mode)
                  ("\\.sch$" . bee-mode)
                  ("\\.scme$" . bee-mode)
                  ("\\.bgl$" . bee-mode)
                  ("\\.bee$" . bee-mode))
                auto-mode-alist)))

(eval-after-load 'bee-mode
  '(setup-python))
