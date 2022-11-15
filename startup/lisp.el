(use-package paredit
  :straight t
  :delight)

(defun rk-start-lisp ()
  (interactive)
  (cond
   ((or (file-exists-p "deps.edn")
        (file-exists-p "project.clj"))
    (call-interactively 'cider-jack-in))
   ((or (file-exists-p "deps.lisp")
        (directory-files "." nil "\\.asd$"))
    (call-interactively 'rk-sbcl))))

(defun rk-lisp-mode-indent-on-save ()
  (make-local-variable 'before-save-hook)
  (add-hook 'before-save-hook
            '(lambda ()
               (lisp-indent-region (point-min) (point-max)))))

(defun hybrid-lisp-indent-function (indent-point state)
  (if (save-excursion
        (cl-loop for pt in (nth 9 state)
                 do (goto-char pt)
                 thereis (looking-at "(\\(labels\\|cl-labels\\|flet\\|cl-flet\\)")))
      (common-lisp-indent-function indent-point state)
    (lisp-indent-function indent-point state)))

(defun setup-lisp-indent-function (indent-function indent-settings)
  (dolist (x indent-settings)
    (put (car x)
         indent-function
         (if (numberp (cdr x))
             (cdr x)
           (get (cdr x) indent-function)))))

(defun rk-yank-simple-escape-quotes ()
  "Same as the `yank' command, inserts the most recently killed
text, additionally escaping any double quotes within the text."
  (interactive)
  (let ((str (substring-no-properties (current-kill 0 t)))
        (start (point))
        (end (make-marker)))
    (insert str)
    (set-marker end (point))
    (save-excursion
      (goto-char start)
      (while (search-forward "\"" (marker-position end) t)
        (replace-match "\\\"" 'fixedcase 'literal)))))

(defun rk-yank-escape-quotes (&optional unescape-p)
  "Like the `yank' command, inserts the most recently killed
text, additionally escaping any double quotes within the
text. When invoked with prefix arg, unescapes - i.e.,removes one
level of escaping from quote chars."
  (interactive "P")
  (let ((str (substring-no-properties (current-kill 0 t)))
        (start (point))
        (end (make-marker)))
    (insert str)
    (set-marker end (point))
    (save-excursion
      (goto-char start)
      (while (search-forward-regexp (if unescape-p
                                        "\\([\\]*\\)\\([\\]\"\\)"
                                      "\"")
                                    (marker-position end)
                                    t)
        (replace-match (if unescape-p
                           (let ((slashes (match-string 1)))
                             (concat (substring slashes
                                                0
                                                (max 0 (- (length slashes) 1)))
                                     "\""))
                         "\\\"")
                       'fixedcase
                       'literal)))))

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
  ;; (rk-lisp-mode-indent-on-save)
  )

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
  ;; (set-common-lisp-block-comment-syntax)
  (set (make-local-variable 'lisp-indent-function)
       'common-lisp-indent-function)
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
  ;; (rk-lisp-mode-indent-on-save)
  (paredit-mode 1))

(use-package lisp-mode
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-m" . pp-macroexpand-last-sexp)
        ("C-c C-p" . pp-eval-last-sexp)
        ("C-c C-k" . eval-buffer)
        ("C-c C-y" . rk-yank-escape-quotes)
        :map lisp-interaction-mode-map
        ("C-c C-m" . pp-macroexpand-last-sexp)
        ("C-c C-p" . pp-eval-last-sexp)
        ("C-c C-k" . eval-buffer)
        ("C-c C-y" . rk-yank-escape-quotes))
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
