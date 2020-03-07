(rk-el-get-bundles
 ag
 diminish
 dired-hacks
 geiser
 gh
 graphviz-dot-mode
 guide-key
 magit
 markdown-mode
 paredit
 plantuml-mode
 projectile
 slime
 window-numbering)

;;;;;;;;;;;;;;;; user-prefix keymap ;;;;;;;;;;;;;;;;

(defun user-commands-prefix-help ()
  (interactive)
  (message "Welcome to the User Commands Prefix map"))

(defvar user-commands-prefix-map (make-sparse-keymap))

(defun set-user-commands-prefix-key (k)
  (global-unset-key k)
  (define-prefix-command 'user-commands-prefix
    'user-commands-prefix-map
    k)
  (define-key global-map k 'user-commands-prefix))
;; (set-user-commands-prefix-key (kbd "C-;"))
(set-user-commands-prefix-key (kbd "\C-\\"))

(defun rk--define-keys (kmap ks)
  (dolist (k ks)
    (let ((key (first k))
          (def (second k)))
      (define-key kmap (kbd key) def))))

(rk--define-keys user-commands-prefix-map
  '(("\C-\\" compile)
    ("." find-tag)
    ("2" rk-2col-view)
    ("3" rk-3col-view)
    ("4" rk-4col-view)
    ("9" rk-fill-vertical-panes)
    ("<" pop-tag-mark)
    ("\C-l" bury-buffer)
    ("g" rk-toggle-debug-on-error)
    ("j" jump-to-register)
    ("l" cider-jack-in)
    ("m" switch-to-mu4e)
    ("o" browse-url-default-browser)
    ("q" switch-back)
    ("r" cider-switch-to-current-repl-buffer)
    ("t" toggle-truncate-lines)
    ("u" browse-url)
    ("v" magit-status)
    ("w" window-configuration-to-register)
    ("W" visual-line-mode)
    ("z" switch-to-app)
    ("\C-z" switch-to-app)
    ("|" rk-toggle-window-split)
    ("\C-c" display-time-world)))

;;;;;;;;;;;;;;;;

(use-package ag
  :config
  (setq ag-reuse-buffers t)
  :bind (("C-c g g" . ag)
         ("C-c g d" . ag-dired)
         ("C-c g p" . ag-project)))

(use-package buffer-move
  :bind (:map user-commands-prefix-map
              ("<left>"  . buf-move-left)
              ("<right>" . buf-move-right)
              ("<down>"  . buf-move-down)
              ("<up>"    . buf-move-up)))

(use-package diary-lib
  :config
  (add-hook 'list-diary-entries-hook 'include-other-diary-files t)
  (add-hook 'diary-hook 'appt-make-list)
  (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
  (add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files))

(defun rk--compilation-mode-colorize-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))

(defun rk-ansi-colorize-region (&optional start end)
  "ANSI colorize a region"
  (interactive (list (mark) (point)))
  (ansi-color-apply-on-region start end))

(use-package ansi-color
  :config
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'compilation-filter-hook 'rk--compilation-mode-colorize-buffer)
  (add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply))

(defun rk--setup-cvs-mode ()
  (font-lock-mode 1))

(defun rk--cvs-load-hook ()
  (setq cvs-buffer-name-alist
        (cons `("diff" ,cvs-diff-buffer-name nil)
              (remove-if '(lambda (x) (equal (car x) "diff"))
                         cvs-buffer-name-alist))))

(use-package pcvs
  :config
  (add-hook 'cvs-mode-hook 'toggle-truncate-lines)
  (add-hook 'cvs-mode-hook 'rk--setup-cvs-mode)
  (add-hook 'pcl-cvs-load-hook 'rk--cvs-load-hook)
  (setq log-edit-keep-buffer t)
  (setenv "CVS_RSH" "ssh"))

(defun rk--magit-setup-hook ()
  (local-unset-key [C-tab])
  (define-key magit-mode-map [C-tab] nil))

(use-package magit
  :config
  (when (facep 'magit-item-highlight)
    (set-face-attribute 'magit-item-highlight nil
                        :background "lightgrey"
                        :foreground "black"))
  (when (facep 'magit-tag)
    (set-face-attribute 'magit-tag nil :foreground "black"))
  (setq magit-last-seen-setup-instructions "1.4.0")
  (add-hook 'magit-mode-hook 'rk--magit-setup-hook))

(defun rk--add-el-get-info-dirs ()
  (require 'find-lisp)
  (let ((local-info-directory (expand-file-name "~/.emacs.d/info")))
    (unless (file-directory-p local-info-directory)
      (mkdir local-info-directory))
    (with-cwd
     local-info-directory
     (dolist (f (find-lisp-find-files "~/.emacs.d/el-get/" "\\.info$"))
       (let ((d (file-name-directory f)))
         (when (directory-files d nil "\\.info$")
           (call-process "install-info"
                         nil
                         '(" *info-setup*" t)
                         nil
                         "--debug"
                         f
                         "dir")
           (add-to-list 'Info-additional-directory-list d)))))
    (add-to-list 'Info-directory-list local-info-directory))
  (add-to-list 'Info-directory-list "/usr/local/share/info"))

(use-package info
  :config
  (set-face-attribute 'info-header-node nil :foreground "black")
  (set-face-attribute 'info-node nil :foreground "black")
  (rk--add-el-get-info-dirs))

(use-package man
  :config
  (setenv "MANPATH"
          (join ":"
                '("/usr/local/share/man/"
                  "/usr/share/man/")))
  (setenv "MANWIDTH" "80")
  (setq Man-fontify-manpage nil))

(use-package outline-mode
  :bind
  (("\C-c\C-e" . show-entry)
   ("C-c +"    . show-entry)
   ("\C-c["    . show-entry)
   ("\C-c\C-a" . show-all)
   ("C-c ("    . show-all)
   ("\C-c{"    . show-all)
   ("\C-c\C-t" . hide-body)
   ("\C-c}"    . hide-body)
   ("C-c )"    . hide-body)
   ("\C-c\C-c" . hide-entry)
   ("C-c -"    . hide-entry)
   ("\C-c]"    . hide-entry))
  :config
  (add-hook 'outline-minor-mode-hook 'setup-outline-minor-mode))

(defun rk--vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 0 name "blame" (if rev (concat  rev)))))

(use-package vc-git
  :config
  (fset 'vc-git-annotate-command 'rk--vc-git-annotate-command))

(use-package winner
  :bind (:map user-commands-prefix-map
              ("\C-b" . winner-undo)
              ("\C-f" . winner-redo))
  :config
  (winner-mode 1))

(use-package projectile
  :config
  (setq projectile-keymap-prefix (kbd "C-c C-p")))

(defun rk-slime-list-connections ()
  (interactive)
  (slime-list-connections)
  (pop-to-buffer "*SLIME Connections*"))

(defun rk-slime-mode-hook ()
  (setq browse-url-browser-function 'rk-url-browser-function)
  ;; (set-face-attribute 'slime-highlight-edits-face nil :background "grey")
  (define-key slime-mode-map "\M-\C-x" 'slime-compile-defun)
  (define-key slime-mode-map "\C-c\C-xc" 'rk-slime-list-connections)
  (unless (boundp 'last-command-char)
    (defvar last-command-char nil)))

(use-package slime
  :config
  (slime-setup '(slime-repl))
  (setq slime-protocol-version 'ignore)
  (add-hook 'slime-mode-hook 'rk-slime-mode-hook))

(defun rk-sbcl ()
  (interactive)
  (if-bind (sbcl-path (locate-path "sbcl" exec-path))
    (let ((slime-lisp-implementations `((sbcl (,sbcl-path)))))
      ;; (setenv "SBCL_HOME" (file-name-directory sbcl-path))
      (slime))
    (error "The sbcl application could not be found")))

(use-package minibuffer
  :config
  (setq completion-cycle-threshold 2
        completion-flex-nospace nil
        completion-pcm-complete-word-inserts-delimiters t
        completion-pcm-word-delimiters "-_./:| "
        completion-show-help nil
        ;; completions-format 'vertical    ; *Completions* buffer
        completions-format 'horizontal
        enable-recursive-minibuffers t
        read-answer-short t
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        resize-mini-windows 'grow-only)
  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1))

;;;;;;;;;;;;;;;; startup ;;;;;;;;;;;;;;;;

(defun rk-toggle-frame-width ()
  "Toggle between narrow and wide frame layouts"
  (interactive)
  (let ((z-wid (aif (assq 'width initial-frame-alist) (cdr it) 162)))
    (if (< (frame-width) z-wid)
        (set-frame-width (selected-frame) z-wid)
      (set-frame-width (selected-frame) 81))))

(defun rk-toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun rk-previous-window ()
  "Switch to previous window"
  (interactive)
  (other-window -1))

(defun rk-next-window ()
  "Switch to next window"
  (interactive)
  (other-window 1))

(defun rk-other-buffer ()
  "Replacement for bury-buffer"
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun rk-kill-files-matching (pattern)
  "Kill all buffers whose filenames match specified regexp"
  (interactive "sRegexp: ")
  (dolist (buffer (buffer-list))
    (let ((file-name (buffer-file-name buffer)))
      (if (and file-name (string-match pattern file-name))
          (kill-buffer buffer)))))

(defun rk-narrow-forward-page (arg)
  (interactive "p")
  (widen)
  (forward-page arg)
  (narrow-to-page))

(defun rk-narrow-backward-page (arg)
  (interactive "p")
  (widen)
  (backward-page (1+ (or arg 1)))
  (narrow-to-page))

(defun rk-toggle-debug-on-error ()
  (interactive)
  (setq debug-on-error (not debug-on-error))
  (message "debug-on-error set to `%s'" debug-on-error))

(defun rk--n-col-view (n)
  "Split the current frame into N vertical windows"
  (let ((cur (selected-window)))
    (save-excursion
      (delete-other-windows)
      (let* ((frame-width-cols (/ (frame-pixel-width) (frame-char-width)))
             (pane-width (round (/ (- frame-width-cols n 1) n))))
        (dotimes (i (1- n))
          (split-window-horizontally pane-width)
          (other-window 1)
          (bury-buffer))
        (balance-windows)))
    (select-window cur)))

(defun rk-4col-view ()
  (interactive)
  (rk--n-col-view 4))

(defun rk-3col-view ()
  (interactive)
  (rk--n-col-view 3))

(defun rk-2col-view ()
  (interactive)
  (rk--n-col-view 2))

(defun rk-fill-vertical-panes ()
  (interactive)
  (delete-other-windows)
  (let ((pane-width 80)
        (cur (selected-window)))
    (save-excursion 
      (dotimes (i (1- (/ (/ (frame-pixel-width) (frame-char-width))
                         pane-width)))
        (split-window-horizontally pane-width)
        (other-window 1)
        (bury-buffer))
      (balance-windows))
    (select-window cur)))

(defun rk-other-window-send-keys (keys)
  (interactive (list (read-key-sequence "Keysequence: ")))
  (let ((window (selected-window)))
    (unwind-protect
        (save-excursion
          (other-window (or current-prefix-arg 1))
          (let ((last-kbd-macro (read-kbd-macro keys)))
            (call-last-kbd-macro)))
      (select-window window))))

(defun rk--get-region-or-read-terms (prompt)
  (replace-regexp-in-string "[ ]+"
                            "+"
                            (or (region-string) (read-string prompt))))

(defun rk--query-string-encode (s)
  (replace-regexp-in-string "[ ]+" "+" s))

(defun rk-google (q)
  (interactive
   (list (rk--query-string-encode (or (region-string)
                                      (read-string "Google: ")))))
  (browse-url
   (concat "https://www.google.com/search?q=" q)))

(defun rk-ddg ()
  (interactive)
  (browse-url
   (concat "https://duckduckgo.com/?q="
           (rk--query-string-encode (or (region-string)
                                        (read-string "DuckDuckGo: "))))))

(defun rk-next-page ()
  (interactive)
  (widen)
  (forward-page)
  (narrow-to-page))

(defun rk-prev-page ()
  (interactive)
  (widen)
  (backward-page 2)
  (narrow-to-page))
