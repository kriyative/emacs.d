(cond
 ((string-match "24\\.3\\." emacs-version)
  (rk-el-get-bundles
   (subr-x
    :type http
    :url "https://raw.githubusercontent.com/emacs-mirror/emacs/master/lisp/emacs-lisp/subr-x.el")
   (clojure-mode :checkout "5.5.2")
   (cider :checkout "v0.17.0")))
 (t
  (rk-el-get-bundles
   clojure-mode
   (cider :checkout "v0.23.0")
   kriyative/git-code-review
   paredit
   slime
   geiser)))

;;;;;;;;;;;;;;;; packages ;;;;;;;;;;;;;;;;

(defun clojure-mode-hook ()
  (auto-revert-mode 1)
  (outline-minor-mode 1)
  (enable-paredit-mode)
  (make-local-variable 'before-save-hook)
  (add-hook 'before-save-hook
            '(lambda ()
               (clojure-indent-region (point-min) (point-max)))))

(use-package clojure-mode
  :demand t
  :bind
  (:map global-map
        ("H-l" . cider-jack-in))
  :bind
  (:map clojure-mode-map
        ("C-c ,"   . cider-test-run-loaded-tests)
        ("C-c M-," . cider-test-run-test)
        ("C-M-x"   . cider-force-eval-defun-at-point)
        ("H-d"     . cider-doc)
        ("H-j"     . cider-javadoc)
        ("H-w"     . cider-grimoire)
        ("H-r"     . cider-switch-to-repl-buffer)
        ("H-l"     . cider-jack-in))
  :config
  (add-hook 'clojure-mode-hook 'clojure-mode-hook)
  (setq auto-mode-alist
        (remove-if (lambda (x)
                     (equal (car x) "\\.cljc\\'"))
                   auto-mode-alist))
  (add-to-list 'auto-mode-alist '("\\.cljc\\'" . clojurec-mode)))

(use-package cider-repl
  :bind
  (("H-r"   . cider-switch-to-last-clojure-buffer))
  :config
  (cider-repl-add-shortcut "sayoonara" 'cider-quit)
  (setq cider-completion-use-context nil
        cider-prompt-for-symbol nil
        cider-repl-display-help-banner nil
        cider-use-overlays nil
        cider-repl-use-pretty-printing nil
        cider-redirect-server-output-to-repl nil))

(defun cider-mode-hook ()
  (eldoc-mode)
  (outline-minor-mode))

;; (remove-hook 'cider-mode-hook 'cider-mode-hook)
;; (unload-feature 'cider t)

(use-package cider
  :bind
  (:map cider-mode-map
        ("C-c C-k" . cider-load-buffer-ext))
  :config
  (add-hook 'cider-mode-hook 'cider-mode-hook)
  (setq cider-lein-parameters "trampoline repl :headless"
        cider-clojure-global-options "-Anrepl:dev"
        cider-clojure-cli-global-options "-Anrepl:dev")
  (add-to-list 'clojure-build-tool-files "deps.edn"))

(defun cider--remove-current-ns (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (cider-tooling-eval
     (format "(remove-ns '%s)" (cider-current-ns))
     (cider-interactive-eval-handler (current-buffer)))))

(defun cider-load-buffer-ext (&optional arg)
  (interactive "p")
  (if (< 1 arg)
      (progn
        (message "Removing namespace: %s" (cider-current-ns))
        (cider--remove-current-ns)
        (cider-load-buffer))
    (cider-load-buffer)))

(defun cider--remove-sym (sym)
  (with-current-buffer (current-buffer)
    (cider-tooling-eval
     (format "(ns-unmap *ns* '%s)" sym)
     (cider-interactive-eval-handler (current-buffer)))))

(defun cider-force-eval-defun-at-point (&optional arg)
  (interactive "p")
  (when (< 1 arg)
    (save-excursion
      (beginning-of-defun)
      (forward-char)
      (forward-sexp 2)
      (backward-sexp)
      (when (string-match "^\\^" (cider-sexp-at-point))
        (forward-sexp 2)
        (backward-sexp))
      (let ((sym (cider-symbol-at-point)))
        (message "Removing sym: %s" sym)
        (cider--remove-sym sym))))
  (cider-eval-defun-at-point nil))

(use-package git-code-review
  :config
  (add-hook 'clojure-mode-hook 'gcr-mode)
  (add-hook 'emacs-lisp-mode-hook 'gcr-mode)
  (add-hook 'common-lisp-mode-hook 'gcr-mode))

(use-package ediff
  :config
  (set 'ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package sql
  :config
  (dolist (pv '((:prompt-regexp "^[-[:alnum:]_]*=[#>] ")
                (:prompt-cont-regexp "^[-[:alnum:]_]*[-(][#>] ")))
    (apply 'sql-set-product-feature 'postgres pv)))

(defun rk-lisp-mode-indent-on-save ()
  (make-local-variable 'before-save-hook)
  (add-hook 'before-save-hook
            '(lambda ()
               (lisp-indent-region (point-min) (point-max)))))

(defun hybrid-lisp-indent-function (indent-point state)
  (cl-labels ((cl-form-p (pts)
                (let (break-loop)
                  (while (and (null break-loop) pts)
                    (let* ((pt (pop pts))
                           (clp ))
                      (save-excursion
                       (goto-char pt)
                       (when (looking-at "(\\(labels\\|cl-labels\\|flet\\|cl-flet\\)")
                         (message "hybrid-lisp-indent-function: found cl-form: %s"
                                  (match-string 1))
                         (setq break-loop t)))))
                  break-loop)))
    (let ((pts (nth 9 state)))
      (if (cl-form-p pts)
          (common-lisp-indent-function indent-point state)
          (lisp-indent-function indent-point state)))))

(defun setup-lisp-indent-function (indent-function indent-settings)
  (dolist (x indent-settings)
    (put (car x)
         indent-function
         (if (numberp (cdr x))
             (cdr x)
           (get (cdr x) indent-function)))))

(defun rk-emacs-lisp-mode-hook ()
  ;; (local-set-key " " 'lisp-complete-symbol)
  (outline-minor-mode 1)
  (setq outline-regexp "^[(;]"
        indent-tabs-mode nil)
  (set (make-local-variable 'lisp-indent-function) 'hybrid-lisp-indent-function)
  (setup-lisp-indent-function 'lisp-indent-function
                              '((if-let . if)))
  (setup-lisp-indent-function 'common-lisp-indent-function
                              '((cl-labels . labels)
                                (cl-flet . flet)
                                (while . when)))
  ;; don't do the following, inconsistent
  ;; (setup-lisp-indent-function)
  (local-set-key "\M-." 'find-function)
  (font-lock-mode 1)
  ;; (auto-complete-mode -1)
  (eldoc-mode 1)
  (paredit-mode 1)
  (rk-lisp-mode-indent-on-save)
  (define-key emacs-lisp-mode-map
    "\C-c\C-p" 'pp-eval-last-sexp)
  (define-key lisp-interaction-mode-map
    "\C-c\C-p" 'pp-eval-last-sexp))

(defun set-common-lisp-block-comment-syntax ()
  (modify-syntax-entry ?# "<1" font-lock-syntax-table)
  (modify-syntax-entry ?| ">2" font-lock-syntax-table)
  (modify-syntax-entry ?| "<3" font-lock-syntax-table)
  (modify-syntax-entry ?# ">4" font-lock-syntax-table))

(defun rk-common-lisp-mode-hook ()
  (font-lock-mode)
  (font-lock-add-keywords 'lisp-mode
                          '(("defclass\*" . font-lock-keyword-face)))
  (modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" lisp-mode-syntax-table)
  (modify-syntax-entry ?\} "){" lisp-mode-syntax-table)
  (set-common-lisp-block-comment-syntax)
  (set (make-local-variable 'lisp-indent-function) 'common-lisp-indent-function)
  (setup-lisp-indent-function 'common-lisp-indent-function
                              '((awhen . 1)
                                (when-let . 1)
                                (aif . if)
                                (if-let . if)
                                (awhile . 1)
                                (while-let . 1)
                                ;; (bind . lambda)
                                (callback . lambda)
                                (c-fficall . with-slots)
                                (with-cwd . 1)
                                (save-values . 1)))
  (setq indent-tabs-mode nil)
  (paredit-mode 1)
  (rk-lisp-mode-indent-on-save))

(use-package lisp-mode
  :bind (:map emacs-lisp-mode-map
              ("C-c C-m" . pp-macroexpand-last-sexp))
  :config
  (add-hook 'emacs-lisp-mode-hook 'rk-emacs-lisp-mode-hook)
  (add-hook 'lisp-interaction-mode-hook 'rk-emacs-lisp-mode-hook)
  (add-hook 'lisp-mode-hook 'rk-common-lisp-mode-hook)
  (add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
  (with-current-buffer "*scratch*"
    (lisp-interaction-mode))
  (info-lookup-add-help
   :mode 'lisp-mode
   :regexp "[^][()'\" \t\n]+"
   :ignore-case t
   :doc-spec '(("(ansicl)Index" nil nil nil)))
  ;; (let* ((ilc-symbol (assoc 'symbol info-lookup-cache))
  ;;        (ilc-lisp-mode (assoc 'lisp-mode ilc-symbol)))
  ;;   (rplacd ilc-symbol (remove ilc-lisp-mode (cdr ilc-symbol))))
  (info-lookup-setup-mode 'symbol 'lisp-mode))

(defun rk-java-mode-hook ()
  (setq tab-width 4))

(use-package cc-mode
  :config
  (add-hook 'java-mode-hook 'rk-java-mode-hook))

(defun rk-slime-list-connections ()
  (interactive)
  (slime-list-connections)
  (pop-to-buffer "*SLIME Connections*"))

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
  (slime-setup '(slime-repl))
  (setq slime-protocol-version 'ignore)
  (add-hook 'slime-mode-hook 'rk-slime-mode-hook))

(defun rk-sbcl ()
  (interactive)
  (if-bind (lisp-path (locate-path "sbcl" exec-path))
    (let ((slime-lisp-implementations `((sbcl (,lisp-path "--dynamic-space-size" "160")))))
      ;; (setenv "SBCL_HOME" (file-name-directory sbcl-path))
      (slime))
    (error "The sbcl application could not be found")))

(defun rk-cloture ()
  (interactive)
  (if-bind (lisp-path "/home/ram/src/cloture/cloture")
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

(defun setup-geiser ()
  )

(use-package geiser
  :config
  (add-hook 'geiser-mode-hook 'setup-geiser))

(defun rk-chez ()
  (interactive)
  (run-geiser 'chez))

(defun rk-guile ()
  (interactive)
  (run-geiser 'guile))

(defun rk-racket ()
  (interactive)
  (run-geiser 'racket))
