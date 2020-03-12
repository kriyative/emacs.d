(rk-el-get-bundles
 (cider :checkout "v0.17.0"))

(if (string-match "24.*" emacs-version)
    (rk-el-get-bundles
     (clojure-mode :checkout "5.5.2"))
  (rk-el-get-bundles clojure-mode))

;;;;;;;;;;;;;;;; packages ;;;;;;;;;;;;;;;;

(defun clojure-mode-hook ()
  (auto-revert-mode 1)
  (outline-minor-mode 1)
  (enable-paredit-mode))

(use-package clojure-mode
  :bind
  (:map clojure-mode-map
        ("C-c ," . cider-test-run-loaded-tests)
        ("C-c M-," . cider-test-run-test)
        ("C-M-x" . cider-force-eval-defun-at-point))
  :config
  (add-hook 'clojure-mode-hook 'clojure-mode-hook)
  (setq auto-mode-alist
        (remove-if (lambda (x)
                     (equal (car x) "\\.cljc\\'"))
                   auto-mode-alist))
  (add-to-list 'auto-mode-alist '("\\.cljc\\'" . clojurec-mode)))

(use-package cider-repl
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

(use-package ediff
  :config
  (set 'ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package sql
  :config
  (dolist (pv '((:prompt-regexp "^[-[:alnum:]_]*=[#>] ")
                (:prompt-cont-regexp "^[-[:alnum:]_]*[-(][#>] ")))
    (apply 'sql-set-product-feature 'postgres pv)))

(defun rk-emacs-lisp-mode-hook ()
  ;; (local-set-key " " 'lisp-complete-symbol)
  (outline-minor-mode 1)
  (setq outline-regexp "^[(;]"
        indent-tabs-mode nil)
  ;; (setup-lisp-indent-function)
  (local-set-key "\M-." 'find-function)
  (font-lock-mode 1)
  ;; (auto-complete-mode -1)
  (eldoc-mode 1))

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

(defun rk-common-lisp-mode-hook ()
  (font-lock-mode)
  (font-lock-add-keywords 'lisp-mode
                          '(("defclass\*" . font-lock-keyword-face)))
  (modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)
  (set (make-local-variable 'lisp-indent-function) 'common-lisp-indent-function)
  (setup-lisp-indent-function 'common-lisp-indent-function)
  (setq indent-tabs-mode nil))

(use-package lisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'rk-emacs-lisp-mode-hook)
  (add-hook 'lisp-interaction-mode-hook 'rk-emacs-lisp-mode-hook)
  (add-hook 'lisp-mode-hook 'rk-common-lisp-mode-hook)
  (add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
  (enable-paredit-mode))
