(my-el-get-bundles
 (cider :checkout "v0.17.0")
 csv-mode)

;;;;;;;;;;;;;;;; packages ;;;;;;;;;;;;;;;;

(defun clojure-mode-hook ()
  (auto-revert-mode 1)
  (outline-minor-mode 1)
  (enable-paredit-mode))

(use-package clojure-mode
  :config
  (add-hook 'clojure-mode-hook 'clojure-mode-hook)
  (add-to-list 'auto-mode-alist '("\\.cljc\\'" . clojure-mode)))

(use-package cider-repl
  :init
  (setq cider-completion-use-context nil
	cider-prompt-for-symbol nil
	cider-repl-display-help-banner nil
	cider-use-overlays nil
	cider-repl-use-pretty-printing nil)
  :bind
  (("\C-c\M-o" . cider-repl-clear-buffer))
  :config
  (cider-repl-add-shortcut "sayoonara" 'cider-quit)
  (setq cider-repl-use-pretty-printing nil))

(defun cider-mode-hook ()
  (eldoc-mode)
  (outline-minor-mode)
  (define-key cider-mode-map "\C-c\C-k" 'cider-load-buffer-ext)
  (define-key cider-mode-map "\C-c," 'cider-test-run-loaded-tests)
  (define-key cider-mode-map "\C-c\M-," 'cider-test-run-test))

;; (remove-hook 'cider-mode-hook 'cider-mode-hook)

;; (unload-feature 'cider t)
(use-package cider
  :config
  (add-hook 'cider-mode-hook 'cider-mode-hook)
  (setq cider-lein-parameters "trampoline repl :headless"
        cider-clojure-global-options "-Anrepl"
        cider-clojure-cli-global-options "-Anrepl")
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

(use-package csv-mode
  :config
  (setq csv-align-style 'auto))

(use-package ediff
  :config
  (set 'ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package geiser
  :config
  (add-hook 'geiser-mode-hook 'setup-geiser))

(defun run-chez ()
  (interactive)
  (run-geiser 'chez))

(defun run-guile ()
  (interactive)
  (run-geiser 'guile))

(defun run-racket ()
  (interactive)
  (run-geiser 'racket))

(use-package graphviz-dot-mode
  :config
  (setq graphviz-dot-view-command "xdot %s"))
