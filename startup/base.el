(defun rk--set-all-line-truncation (v)
  (set-default 'truncate-lines t)
  (set-default 'truncate-partial-width-windows t)
  (set-default 'line-move-visual nil)
  (make-local-variable 'truncate-lines)
  (setq truncate-lines v)
  (make-local-variable 'truncate-partial-width-windows)
  (setq truncate-partial-width-windows v))

(defun error-not-implemented-for-system (fname)
  (error
   (format "No implementation for '%s' on %S" fname system-type)))

(defun as-list (x)
  (if (consp x) x (list x)))

(defun rk-bind-keys (bindings &rest keymaps)
  (dolist (keymap (or keymaps (list :global)))
    (dolist (binding bindings)
      (let* ((key (first binding))
             (key (if (stringp key)
                      (kbd key)
                    key))
             (def (second binding)))
        (if (eq :global keymap)
            (progn
              (global-unset-key key)
              (when def
                (global-set-key key def)))
          (define-key (if (symbolp keymap)
                          (symbol-value keymap)
                        keymap)
            key def))))))

(defun rk--confirm-exit ()
  (y-or-n-p "Exit Emacs, Are you sure? "))

(defun rk--message-with-timestamp (old-func fmt-string &rest args)
  "Prepend current timestamp (with microsecond precision) to a message"
  (when (and fmt-string (< 0 (length fmt-string)))
    (apply old-func
           (concat "[" (format-time-string "%m-%d %T.%3N")
                   " " (buffer-name (current-buffer))
                   "] " fmt-string)
           args)))

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

(defun rk--file-age (file)
  (float-time
   (time-subtract (current-time)
                  (file-attribute-modification-time
                   (file-attributes file)))))

(defmacro rk-def-prefix-command (prefix map prefix-key)
  `(progn
     (global-unset-key ,prefix-key)
     (define-prefix-command ,prefix ,map ,prefix-key)
     (define-key global-map ,prefix-key ,prefix)))

;;;;;;;;;;;;;;;; prefix keys ;;;;;;;;;;;;;;;;

;;; user-prefix keymap

(defvar user-commands-prefix-map (make-sparse-keymap))

(rk-def-prefix-command 'user-commands-prefix
                       'user-commands-prefix-map
                       (kbd "\C-\\"))

;;;;;;;;;;;;;;;; packages ;;;;;;;;;;;;;;;;

(use-package efun-cmds
  :straight (efun-cmds :type git
		       :host github
		       :repo "kriyative/emacs-fun"
                       :build (:not compile)))

(use-package ibuffer
  :config
  (setq ibuffer-expert t)
  :bind
  (:map ctl-x-map ("C-b" . ibuffer)))

(use-package vc
  :config
  (setq vc-mistrust-permissions t
        vc-initial-comment t
        vc-consult-headers nil
        vc-make-backup-files t
        vc-display-status nil
        vc-follow-symlinks t))

(use-package comint
  :config
  (setq comint-buffer-maximum-size 1024)
  (add-to-list 'comint-output-filter-functions 'shell-strip-ctrl-m)
  (add-to-list 'comint-output-filter-functions 'comint-truncate-buffer))

(use-package telnet
  :config
  (setq telnet-remote-echoes nil))

(use-package dired
  :config
  (setq dired-listing-switches "-alh"
        dired-recursive-copies 'always))

(defun rk--dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (cl-case system-type
      (gnu/linux (call-process "xdg-open" nil 0 nil file))
      (t
       (error-not-implemented-for-system 'rk--dired-open-file)))))

(use-package dired-x
  :after dired
  :demand t
  :config
  (set-default 'dired-omit-mode t)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

  :bind
  (:map dired-mode-map
        ("k" . dired-kill-subdir)
        (">" . dired-omit-mode)
        ([C-return] . rk--dired-open-file)
        ("C-c o" . rk--dired-open-file)))

(use-package imenu
  :bind
  (("C-c C-i" . imenu)))

(use-package term)

(use-package ispell
  :config
  (setq ispell-program-name (locate-path "aspell" exec-path)))

(use-package shell
  :config
  (when (file-exists-p (expand-file-name "~/.bash_profile"))
    (setq explicit-bash-args
          '("--login" "--init-file" "~/.bash_profile" "-i"))))

(use-package dictionary
  :straight (dictionary :type git
			:host github
			:repo "myrkr/dictionary-el")
  :config
  ;; backward compatibility function to support some ancient version of
  ;; dictionary.el
  (unless (fboundp 'process-kill-without-query)
    (defun process-kill-without-query (process)
      (set-process-query-on-exit-flag process nil)))
  :bind
  (("C-c s" . dictionary-search)
   ("C-c m" . dictionary-match-words)))

(use-package wgrep :straight t)

(use-package ag
  :straight t
  :config
  (setq ag-reuse-buffers t)
  :bind (("C-c g g" . ag)
         ("H-g g" . ag)
         ("C-c g d" . ag-dired)
         ("C-c g f" . ag-dired)
         ("H-g d" . ag-dired)
         ("C-c g p" . ag-project)
         ("C-c g P" . ag-project-dired)
         ("H-g p" . ag-project)
         ("C-c g r" . ag-regexp)
         ("H-g r" . ag-regexp)))

(use-package  buffer-move
  :straight (buffer-move :build (:not compile))
  :after (cl)
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

(defun rk--add-info-dirs ()
  (require 'find-lisp)
  (add-to-list 'Info-directory-list "/usr/local/share/info")
  (add-to-list 'Info-directory-list (expand-file-name "~/share/info")))

(use-package info
  :config
  (set-face-attribute 'info-header-node nil :foreground "black")
  (set-face-attribute 'info-node nil :foreground "black")
  (rk--add-info-dirs))

(use-package info-look)

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
  (:map outline-mode-map
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
         ("\C-c]"    . hide-entry)))
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

(use-package minibuffer
  :config
  (setq completion-cycle-threshold nil
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

(use-package password-mode :straight t)

(defun iso-calendar ()
  (interactive)
  (setq european-calendar-style nil)
  (setq calendar-date-display-form
        '(year
          "-"
          (if (< (length month) 2) (concat "0" month) month)
          "-"
          (if (< (length day) 2) (concat "0" day) day)))
  (setq diary-date-forms
        '((year "-" month "-" day "[^0-9]")
          (month "/" day "[^/0-9]")
          (month "/" day "/" year "[^0-9]")
          (monthname " *" day "[^,0-9]")
          (monthname " *" day ", *" year "[^0-9]")
          (dayname "\\W")))
  (cond
   ((string-match "^2[12]" emacs-version)
    (update-calendar-mode-line))
   (t
    (when (fboundp 'calendar-update-mode-line)
      (calendar-update-mode-line)))))

(use-package calendar
  :config
  (iso-calendar)
  (add-hook 'diary-display-hook 'fancy-diary-display)
  ;; (add-hook 'calendar-load-hook 'mark-diary-entries)
  (add-hook 'list-diary-entries-hook 'sort-diary-entries t)
  (setq display-time-day-and-date nil
        display-time-world-list '(("Pacific/Honolulu" "Honolulu")
                                  ("America/Anchorage" "Anchorage")
                                  ("America/Los_Angeles" "Los Angeles")
                                  ("America/Phoenix" "Phoenix")
                                  ("America/Chicago" "Chicago")
                                  ("America/New_York" "New York")
                                  ("Europe/London" "London")
                                  ("Europe/Paris" "Paris")
                                  ("Asia/Calcutta" "Calcutta")
                                  ("Asia/Singapore" "Singapore")
                                  ("Australia/Sydney" "Sydney")
                                  ("Pacific/Auckland" "Auckland"))
        display-time-world-time-format "%a %d %b %R %Z"))

(use-package edit-server
  :straight t
  :config
  (setq edit-server-default-major-mode 'normal-mode
        edit-server-new-frame nil)
  (edit-server-start))

(use-package emacs
  :config
  ;;;;;;;;;;;;;;;; charset encoding ;;;;;;;;;;;;;;;;
  (prefer-coding-system       'utf-8)
  (set-file-name-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-buffer-file-coding-system 'utf-8)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
        browse-url-browser-function 'eww-browse-url)

  (put 'narrow-to-page 'disabled nil)
  (put 'erase-buffer 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)

  ;;;;;;;;;;;;;;;; hooks ;;;;;;;;;;;;;;;;
  (add-to-list 'kill-emacs-query-functions 'rk--confirm-exit)

  ;;;;;;;;;;;;;;;; run at startup ;;;;;;;;;;;;;;;;

  (display-time-mode -1)
  (appt-activate 1)

  (advice-add 'message :around #'rk--message-with-timestamp)
  ;; (message "hello")

  :bind
  (("M-g" . goto-line)
   ("M-`" . next-error)
   ("C-M-`" . previous-error)
   ("\r" . newline-and-indent)
   ("\C-xn" . rk-next-window)
   ("\C-xp" . rk-previous-window)
   ("H-t" . transpose-lines)
   ("C-M-l" . rk-other-buffer)
   ;; ("\C-\\" compile)
   ;; ("\C-xb" iswitchb-buffer)
   ;; ("\C-xb" switch-to-buffer)
   ([C-M-left] . previous-buffer)
   ([C-M-right] . next-buffer)
   ([?\C-.] . tags-search)
   ([?\C-,] . tags-loop-continue)
   
   ("<f5> f" . search-forward)
   ("<f5> b" . search-backward)
   ("<f6>" . previous-error)
   ("<f7>" . next-error)
   ("C-=" . text-scale-increase)
   ("C--" . text-scale-decrease)
   ("H-l" . rk-start-lisp)

   ("M-i" . completion-at-point)
   ("M-j" . jump-to-register)
   ("M-k" . kill-sentence)
   ("M-l" . downcase-dwim)
   ("M-a" . beginning-of-line)
   ("M-e" . end-of-line)
   ("M-o" . other-window)

   :map ctl-x-map
   ("C-f" . x-find-file)

   :map ctl-x-4-map
   ("k" . other-window-send-keys)

   :map user-commands-prefix-map
   ("\C-\\" . compile)
   ("." . find-tag)
   ("2" . rk-2col-view)
   ("3" . rk-3col-view)
   ("4" . rk-4col-view)
   ("9" . rk-fill-vertical-panes)
   ("<" . pop-tag-mark)
   ("\C-l" . bury-buffer)
   ("g" . rk-toggle-debug-on-error)
   ("j" . jump-to-register)
   ("l" . cider-jack-in)
   ("m" . switch-to-mu4e)
   ("o" . browse-url-default-browser)
   ("q" . switch-back)
   ("r" . cider-switch-to-current-repl-buffer)
   ("t" . toggle-truncate-lines)
   ("u" . browse-url)
   ("v" . magit-status)
   ("w" . window-configuration-to-register)
   ("W" . visual-line-mode)
   ("z" . switch-to-app)
   ("\C-z" . switch-to-app)
   ("|" . rk-toggle-window-split)
   ("\C-c" . display-time-world)
   ("+" . rk--x-zoom-in)
   ("-" . rk--x-zoom-out)
   ("0" . rk--x-zoom-reset)

   :map minibuffer-local-completion-map
   ([tab] . minibuffer-complete)
   ([spc] . minibuffer-complete-word)
   ([tab] . minibuffer-complete)
   ([spc] . minibuffer-complete-word)))
