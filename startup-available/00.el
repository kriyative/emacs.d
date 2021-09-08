;;;;;;;;;;;;;;;; el-get initialization ;;;;;;;;;;;;;;;;

(require 'url)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

(require 'el-get)

(defmacro rk-el-get-bundles (&rest rcps)
  `(progn
     ,@(mapcar
        (lambda (rcp)
          `(el-get-bundle ,@(if (listp rcp) rcp (list rcp))))
        rcps)))

;;;;;;;;;;;;;;;; regular startup ;;;;;;;;;;;;;;;;

(rk-el-get-bundles
 alert
 buffer-move
 (emacs-fun :url "https://github.com/kriyative/emacs-fun.git"
            :features (efun-base efun-cmds))
 popup
 use-package)

(defun rk--set-all-line-truncation (v)
  (make-local-variable 'truncate-lines)
  (setq truncate-lines v)
  (make-local-variable 'truncate-partial-width-windows)
  (setq truncate-partial-width-windows v))

(defun error-not-implemented-for-system (fname)
  (error
   (format "No implementation for '%s' on %S" fname system-type)))

(defun rk-bind-keys (bindings &optional keymap)
  (dolist (binding bindings)
    (let* ((key (first binding))
           (key (if (stringp key)
                    (kbd key)
                  key))
           (def (second binding)))
      (if keymap
          (define-key keymap key def)
        (global-unset-key key)
        (when def
          (global-set-key key def))))))

;;;;;;;;;;;;;;;; packages ;;;;;;;;;;;;;;;;

(use-package ibuffer
  :config
  (setq ibuffer-expert t)
  (rk-bind-keys '(("\C-x\C-b" ibuffer))))

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

(defun rk--setup-dired-x ()
  (define-key dired-mode-map (kbd "C-c o") 'rk--dired-open-file))

(use-package dired-x
  :after dired
  :demand t
  :config
  (set-default 'dired-omit-mode t)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
  (rk--setup-dired-x)
  :bind (:map dired-mode-map
              ("k" . dired-kill-subdir)
              (">" . dired-omit-mode)
              ([C-return] . rk--dired-open-file)))

(use-package efun-cmds)

(use-package imenu
  :config
  (rk-bind-keys '(("\C-c\C-i" imenu))))

(use-package term)

;;;;;;;;;;;;;;;; startup ;;;;;;;;;;;;;;;;

(setq inhibit-startup-message t
      inhibit-splash-screen t
      backup-inhibited t
      remote-shell-program "ssh"
      truncate-partial-width-windows t
      visible-bell 'top-bottom
      url-proxy-services nil
      compilation-scroll-output t
      transient-mark-mode t
      shell-file-name "bash"
      max-mini-window-height 0.25
      ;; completion-styles '(basic partial-completion)
      completion-styles '(basic partial-completion substring)
      completion-ignore-case t
      completion-cycle-threshold nil
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completion-ignored-extensions (nconc completion-ignored-extensions
                                           '(".fasl"
                                             ".dfsl"
                                             ".x86f"
                                             ".err"
                                             ".ufasl"
                                             ".DS_Store"))
      mc-gpg-path (locate-path "gpg" exec-path)
      ispell-program-name (locate-path "aspell" exec-path)
      auto-window-vscroll nil)

;;;;;;;;;;;;;;;; charset encoding ;;;;;;;;;;;;;;;;

(prefer-coding-system       'utf-8)
(set-file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(put 'narrow-to-page 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;;;;;;;;;;;;;;; hooks ;;;;;;;;;;;;;;;;

(defun rk--confirm-exit ()
  (y-or-n-p "Exit Emacs, Are you sure? "))

(add-to-list 'kill-emacs-query-functions 'rk--confirm-exit)

;;;;;;;;;;;;;;;; ctl-c-prefix keymap ;;;;;;;;;;;;;;;;

(defun ctl-c-commands-prefix-help ()
  (interactive)
  (message "Welcome to the Ctl-c Commands Prefix map"))

(defvar ctl-c-commands-prefix-map (make-sparse-keymap))

(global-unset-key "\C-c")
(define-prefix-command 'ctl-c-commands-prefix 'ctl-c-commands-prefix-map "\C-c")
(define-key global-map "\C-c" 'ctl-c-commands-prefix)

;;;;;;;;;;;;;;;; keys ;;;;;;;;;;;;;;;;

(rk-bind-keys
 '(("M-g" goto-line)
   ("M-`" next-error)
   ("C-M-`" previous-error)
   ("\r" newline-and-indent)
   ("\C-xn" rk-next-window)
   ("\C-xp" rk-previous-window)
   ("H-t" transpose-lines)
   ("C-M-l" rk-other-buffer)
   ;; ("\C-\\" compile)
   ;; ("\C-xb" iswitchb-buffer)
   ;; ("\C-xb" switch-to-buffer)
   ([C-M-left] previous-buffer)
   ([C-M-right] next-buffer)
   ([?\C-.] tags-search)
   ([?\C-,] tags-loop-continue)
   ("\C-x\C-f" x-find-file)
   ("<f7>" next-error)
   ("C-=" text-scale-increase)
   ("C--" text-scale-decrease)
   ("H-l" rk-start-lisp)

   ("M-i" completion-at-point)
   ("M-j" jump-to-register)
   ("M-k" kill-sentence)
   ("M-l" downcase-dwim)
   ("M-a" beginning-of-line)
   ("M-e" end-of-line)
   ("M-o" other-window)))

(rk-bind-keys
 `(([tab] minibuffer-complete)
   ([spc] minibuffer-complete-word)
   ([tab] minibuffer-complete)
   ([spc] minibuffer-complete-word))
 minibuffer-local-completion-map)

(define-key ctl-x-4-map "k" 'other-window-send-keys)

;;;;;;;;;;;;;;;; run at startup ;;;;;;;;;;;;;;;;

(display-time-mode -1)
(appt-activate 1)

(set-default 'truncate-lines t)
(set-default 'truncate-partial-width-windows t)
(set-default 'line-move-visual nil)

;; raise the limit for bindings and unwind-protect contexts
(setq max-specpdl-size 5000)

(when (file-exists-p (expand-file-name "~/.bash_profile"))
  (setq explicit-bash-args '("--login" "--init-file" "~/.bash_profile" "-i")))

(setq custom-file "~/.emacs.d/custom.el")

(defun message-with-timestamp (old-func fmt-string &rest args)
  "Prepend current timestamp (with microsecond precision) to a message"
  (when (and fmt-string (< 0 (length fmt-string)))
    (apply old-func
           (concat "[" (format-time-string "%m-%d %T.%3N")
                   " " (buffer-name (current-buffer))
                   "] " fmt-string)
           args)))

(advice-add 'message :around #'message-with-timestamp)
;; (message "hello")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; base functions

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

;;; backward compatibility function to support some ancient version of
;;; dictionary.el
(unless (fboundp 'process-kill-without-query)
  (defun process-kill-without-query (process)
    (set-process-query-on-exit-flag process nil))
  )
