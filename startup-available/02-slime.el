(rk-el-get-bundles slime)

(defun rk-slime-list-connections ()
  (interactive)
  (slime-list-connections)
  (pop-to-buffer "*slime-connections*"))

(defun rk-slime-mode-hook ()
  (setq common-lisp-hyperspec-root "file:///usr/share/doc/hyperspec/")
  ;; (set-face-attribute 'slime-highlight-edits-face nil :background "grey")
  (define-key slime-mode-map "\M-\C-x" 'slime-compile-defun)
  (define-key slime-mode-map "\C-c\C-xc" 'rk-slime-list-connections)
  (unless (boundp 'last-command-char)
    (defvar last-command-char nil)))

(defvar rk-slime-last-expression-stack nil)
(setq rk-slime-last-expression-stack nil)
(defun rk-slime-eval-last-expression ()
  (interactive)
  (push (slime-last-expression) rk-slime-last-expression-stack)
  (slime-eval-last-expression))

(defun rk-slime-eval-pick-last-expression (&optional expr)
  (interactive
   (list
    (if (= (length rk-slime-last-expression-stack) 1)
        (first rk-slime-last-expression-stack)
      (completing-read "Expression: " rk-slime-last-expression-stack))))
  (slime-interactive-eval expr))

(use-package slime
  :config
  ;; (setq slime-contribs '(slime-fancy slime-help slime-info))
  (slime-setup '(slime-repl))
  (setq slime-protocol-version 'ignore)
  (add-hook 'slime-mode-hook 'rk-slime-mode-hook))

(defun rk-sbcl ()
  (interactive)
  (if-bind (lisp-path (locate-path "sbcl" exec-path))
    (let ((slime-lisp-implementations
           `((sbcl (,lisp-path
                    ;; "--dynamic-space-size" "512"
                    ;; "--control-stack-size" "64"
                    )))))
      ;; (setenv "SBCL_HOME" (file-name-directory sbcl-path))
      (slime))
    (error "The sbcl application could not be found")))

(defun rk-cloture ()
  (interactive)
  (if-bind (lisp-path (locate-path "cloture" exec-path))
    (let ((inferior-lisp-program cloture-path))
      (slime))
    (error "The cloture application could not be found")))

(defun rk-clisp ()
  (interactive)
  (if-bind (lisp-path (locate-path "clisp" exec-path))
    (let ((slime-lisp-implementations `((clisp (,lisp-path)))))
      (slime))
    (error "The clisp application could not be found")))

(defun rk-ecl ()
  (interactive)
  (if-bind (lisp-path (locate-path "ecl" exec-path))
    (let ((slime-lisp-implementations `((ecl (,lisp-path)))))
      (slime))
    (error "The ecl application could not be found")))

