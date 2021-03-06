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
   (cider :checkout "v0.23.0"))))

(defun clojure-mode-hook ()
  (auto-revert-mode 1)
  (outline-minor-mode 1)
  (enable-paredit-mode)
  (make-local-variable 'before-save-hook)
  ;; (rk-lisp-mode-indent-on-save)
  )

(defvar rk-outline-visibility nil)

(defun rk-outline-toggle ()
  (interactive)
  (unless (boundp 'rk-outline-visibility)
    (make-local-variable 'rk-outline-visibility)
    (setq rk-outline-visibility nil))
  (if (eq 'visible rk-outline-visibility)
      (progn
        (outline-hide-body)
        (set 'rk-outline-visibility 'hidden))
    (outline-show-all)
    (set 'rk-outline-visibility 'visible)))

(use-package clojure-mode
  :demand t
  :bind
  (:map clojure-mode-map
        ("C-c ,"   . cider-test-run-loaded-tests)
        ("C-c M-," . cider-test-run-test)
        ("C-M-x"   . cider-force-eval-defun-at-point)
        ("H-d"     . cider-doc)
        ("H-j"     . cider-javadoc)
        ("H-w"     . cider-grimoire)
        ("H-r"     . cider-switch-to-repl-buffer)
        ("H-l"     . cider-jack-in)
        ("<backtab>" . rk-outline-toggle))
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
