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
  (setq c-basic-offset 2
	tab-width 2
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
