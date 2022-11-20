(defun clojure-mode-hook ()
  (auto-revert-mode 1)
  (outline-minor-mode 1)
  (enable-paredit-mode)
  (make-local-variable 'before-save-hook)
  (setq outline-heading-alist nil
        outline-regexp "^[(;]")
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
  :straight t
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
        ("<backtab>" . rk-outline-toggle)
        ("C-c C-y" . rk-yank-unescape-quotes)
        ("C-c p g" . projectile-grep))
  :config
  (add-hook 'clojure-mode-hook 'clojure-mode-hook)
  (add-hook 'clojure-mode-hook (lambda () (projectile-mode)))
  (setq auto-mode-alist (cl-remove-if (lambda (x)
                                        (equal (car x) "\\.cljc\\'"))
                                      auto-mode-alist))
  (add-to-list 'auto-mode-alist '("\\.cljc\\'" . clojurec-mode)))

(defun cider-mode-hook ()
  (eldoc-mode)
  (outline-minor-mode))

;; (remove-hook 'cider-mode-hook 'cider-mode-hook)
;; (unload-feature 'cider t)

(use-package cider
  :straight t
  :bind
  (:map cider-mode-map
        ("C-c C-k" . cider-load-buffer-ext))
  :config
  (add-hook 'cider-mode-hook 'cider-mode-hook)
  (setq cider-lein-parameters "trampoline repl :headless"
        cider-clojure-cli-global-options "-Adev")
  (add-to-list 'clojure-build-tool-files "deps.edn"))

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

(defvar rk-nrepl-buffer-maximum-size 1000)

(defun rk-nrepl-server-filter-after (process output)
  (let ((server-buffer (process-buffer process)))
    (when (buffer-live-p server-buffer)
      (with-current-buffer server-buffer
        (let ((comint-buffer-maximum-size rk-nrepl-buffer-maximum-size))
          (comint-truncate-buffer))))))

(advice-add 'nrepl-server-filter :after #'rk-nrepl-server-filter-after)

(defun custom-cider-jack-in-cljs (params)
  (interactive "P")
  (let ((process-environment (cl-copy-list process-environment)))
    (setenv "JAVA_HOME" "/usr/lib/jvm/java-11-openjdk-amd64")
    (setenv "PATH" (concat "/usr/lib/jvm/java-11-openjdk-amd64/bin:"
                           (getenv "PATH")))
    (cider-jack-in-cljs params)))

(provide 'init-clojure)
