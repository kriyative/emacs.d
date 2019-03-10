(my-el-get-bundles
 csv-mode
 diminish
 dired-hacks
 emacs-w3m
 geiser
 gh
 graphviz-dot-mode
 guide-key
 magit
 magit-gh-pulls
 (magit-todos :url "https://github.com/alphapapa/magit-todos.git"
              :features magit-todos)
 markdown-mode
 paredit
 plantuml-mode
 projectile
 slime
 window-numbering)

;;;;;;;;;;;;;;;;

(use-package diary-lib
  :config
  (add-hook 'list-diary-entries-hook 'include-other-diary-files t)
  (add-hook 'diary-hook 'appt-make-list)
  (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
  (add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)
  (when (file-exists-p diary-file)
    (diary 0)))

(use-package ansi-color
  :config
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'compilation-filter-hook 'compilation-mode-colorize-buffer)
  (add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply))

(defun setup-cvs-mode ()
  (font-lock-mode 1))

(defun cvs-load-hook ()
  (setq cvs-buffer-name-alist
	(cons `("diff" ,cvs-diff-buffer-name nil)
	      (remove-if '(lambda (x) (equal (car x) "diff"))
			 cvs-buffer-name-alist))))

(use-package pcvs
  :config
  (add-hook 'cvs-mode-hook 'turn-on-line-truncation)
  (add-hook 'cvs-mode-hook 'setup-cvs-mode)
  (add-hook 'pcl-cvs-load-hook 'cvs-load-hook)
  (setq log-edit-keep-buffer t)
  (setenv "CVS_RSH" "ssh"))

(defun magit-setup-hook ()
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
  (add-hook 'magit-mode-hook 'magit-setup-hook))

(use-package magit-gh-pulls
  :disabled t
  :config
  (add-hook 'magit-mode-hook 'magit-gh-pulls-mode))

(use-package magit-todos
  :config
  (setq magit-todos-ignore-case t)
  (add-hook 'magit-mode-hook 'magit-todos-mode))

(defun add-el-get-info-dirs ()
  (require 'find-lisp)
  (let ((local-info-directory (expand-file-name "~/.emacs.d/info")))
    (unless (file-directory-p local-info-directory)
      (mkdir local-info-directory))
    (with-cwd local-info-directory
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
  (add-to-list 'Info-directory-list "/app/stumpwm/share/info")
  (add-to-list 'Info-directory-list "/app/sbcl/share/info")
  (add-to-list 'Info-directory-list "/usr/local/share/info"))

(use-package info
  :config
  (set-face-attribute 'info-header-node nil :foreground "black")
  (set-face-attribute 'info-node nil :foreground "black")
  (add-el-get-info-dirs))

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

(use-package vc
  :config
  (setq vc-mistrust-permissions t
        vc-initial-comment t
        vc-consult-headers nil
        vc-make-backup-files t))

(defun alt-vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 0 name "blame" (if rev (concat  rev)))))

(use-package vc-git
  :config
  (fset 'vc-git-annotate-command 'alt-vc-git-annotate-command))

;;;;;;;;;;;;;;;; ctrl-; keymap ;;;;;;;;;;;;;;;;

(defun ctl-semicolon-help ()
  (interactive)
  (message "Welcome to Ctl-;"))

(defvar ctl-semicolon-map (make-sparse-keymap))

(let ((k (kbd "C-;")))
  (global-unset-key k)
  (define-prefix-command 'ctl-semicolon-prefix 'ctl-semicolon-map k)
  (define-key global-map k 'ctl-semicolon-prefix))

(define-key ctl-semicolon-map "." 'find-tag)
(define-key ctl-semicolon-map "0" 'go-home)
(define-key ctl-semicolon-map "2" '2col-view)
(define-key ctl-semicolon-map "3" '3col-view)
(define-key ctl-semicolon-map "4" '4col-view)
(define-key ctl-semicolon-map "9" 'fill-vertical-panes)
(define-key ctl-semicolon-map "<" 'pop-tag-mark)
(define-key ctl-semicolon-map "\C-b" 'winner-undo)
(define-key ctl-semicolon-map "\C-f" 'winner-redo)
(define-key ctl-semicolon-map "\C-l" 'bury-buffer)
(define-key ctl-semicolon-map "g" 'toggle-debug-on-error)
(define-key ctl-semicolon-map "j" 'jump-to-register)
(define-key ctl-semicolon-map "l" 'cider-jack-in)
(define-key ctl-semicolon-map "m" 'switch-to-mu4e)
(define-key ctl-semicolon-map "o" 'browse-url-default-browser)
(define-key ctl-semicolon-map "q" 'switch-back)
(define-key ctl-semicolon-map "r" 'cider-switch-to-current-repl-buffer)
(define-key ctl-semicolon-map "t" 'toggle-truncate-lines)
(define-key ctl-semicolon-map "u" 'browse-url)
(define-key ctl-semicolon-map "v" 'magit-status)
(define-key ctl-semicolon-map "w" 'window-configuration-to-register)
(define-key ctl-semicolon-map "W" 'visual-line-mode)
(define-key ctl-semicolon-map "z" 'switch-to-app)
(define-key ctl-semicolon-map "\C-z" 'switch-to-app)
(define-key ctl-semicolon-map "|" 'toggle-window-split)
(define-key ctl-semicolon-map [left]  'buf-move-left)
(define-key ctl-semicolon-map [right] 'buf-move-right)
(define-key ctl-semicolon-map [down]  'buf-move-down)
(define-key ctl-semicolon-map [up]    'buf-move-up)
(define-key ctl-semicolon-map (kbd "SPC") 'emms-pause)
(define-key ctl-semicolon-map "e" nil)
(define-key ctl-semicolon-map (kbd "en") 'emms-next)
(define-key ctl-semicolon-map (kbd "ep") 'emms-previous)
(define-key ctl-semicolon-map "\C-e" 'switch-to-emms)
(define-key ctl-semicolon-map "\C-c" 'display-time-world)

;; (define-key dired-mode-map [C-return] 'dired-open-file)

;;;;;;;;;;;;;;;; startup ;;;;;;;;;;;;;;;;

(winner-mode 1)

(defun toggle-frame-width ()
  "Toggle between narrow and wide frame layouts"
  (interactive)
  (let ((z-wid (aif (assq 'width initial-frame-alist) (cdr it) 162)))
    (if (< (frame-width) z-wid)
	(set-frame-width (selected-frame) z-wid)
      (set-frame-width (selected-frame) 81))))

(defun toggle-window-split ()
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

(defun my-previous-window ()
  "Switch to previous window"
  (interactive)
  (other-window -1))

(defun my-next-window ()
  "Switch to next window"
  (interactive)
  (other-window 1))

(defun my-other-buffer ()
  "Replacement for bury-buffer"
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun kill-files-matching (pattern)
  "Kill all buffers whose filenames match specified regexp"
  (interactive "sRegexp: ")
  (dolist (buffer (buffer-list))
    (let ((file-name (buffer-file-name buffer)))
      (if (and file-name (string-match pattern file-name))
	  (kill-buffer buffer)))))

(defun narrow-forward-page (arg)
  (interactive "p")
  (widen)
  (forward-page arg)
  (narrow-to-page))

(defun narrow-backward-page (arg)
  (interactive "p")
  (widen)
  (backward-page (1+ (or arg 1)))
  (narrow-to-page))

(defun toggle-debug-on-error ()
  (interactive)
  (setq debug-on-error (not debug-on-error))
  (message "debug-on-error set to `%s'" debug-on-error))

(defun 4col-view ()
  (interactive)
  (n-col-view 4))

(defun 3col-view ()
  (interactive)
  (n-col-view 3))

(defun 2col-view ()
  (interactive)
  (n-col-view 2))

(defun dev-split-view ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally 85)
  (save-excursion
    (other-window 1)
    (split-window-vertically)
    (split-window-horizontally (/ (window-width) 2))))

(defun fill-vertical-panes ()
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

(defun turn-on-line-truncation ()
  (interactive)
  (set-all-line-truncation t))

(defun other-window-send-keys (keys)
  (interactive (list (read-key-sequence "Keysequence: ")))
  (let ((window (selected-window)))
    (unwind-protect
        (save-excursion
          (other-window (or current-prefix-arg 1))
          (let ((last-kbd-macro (read-kbd-macro keys)))
            (call-last-kbd-macro)))
      (select-window window))))

(defun get-region-or-read-terms (prompt)
  (replace-regexp-in-string "[ ]+"
                            "+"
                            (or (region) (read-string prompt))))

(defun query-string-encode (s)
  (replace-regexp-in-string "[ ]+" "+" s))

(defun google (q)
  (interactive
   (list (query-string-encode (or (region) (read-string "Google: ")))))
  (browse-url
   (concat "https://www.google.com/search?q=" q)))

(defun ddg ()
  (interactive)
  (browse-url
   (concat "https://duckduckgo.com/?q="
           (query-string-encode (or (region) (read-string "DuckDuckGo: "))))))

(defun mdn (q)
  (interactive
   (list (query-string-encode (or (region) (read-string "MDN: ")))))
  (google (concat "site:developer.mozilla.org " q)))

(defun wikipedia ()
  (interactive)
  (browse-url
   (concat "http://en.wikipedia.org/w/index.php?search="
	   (query-string-encode
            (capitalize (or (region) (read-string "Wikipedia: ")))))))

(defun emacswiki (q)
  (interactive (list (get-region-or-read-terms "emacswiki: ")))
  (browse-url
   (concat "http://www.emacswiki.org/emacs/Search?action=index&match="
           (query-string-encode q))))

(defun dired-open-file ()
  "In Dired, open a file using its default application."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (unless (file-directory-p file)
      (message "Opening %s..." file)
      (call-process "xdg-open" nil 0 nil file))))

(defun next-page ()
  (interactive)
  (widen)
  (forward-page)
  (narrow-to-page))

(defun prev-page ()
  (interactive)
  (widen)
  (backward-page 2)
  (narrow-to-page))

(defun rmail-mime-buffer ()
  "MIME decode the contents of the current buffer."
  (interactive)
  (let* ((data (buffer-string))
         (buf (get-buffer-create "*RMAIL*"))
         (rmail-mime-mbox-buffer rmail-view-buffer)
         (rmail-mime-view-buffer buf))
    (set-buffer buf)
    (setq buffer-undo-list t)
    (let ((inhibit-read-only t))
      ;; Decoding the message in fundamental mode for speed, only
      ;; switching to rmail-mime-mode at the end for display.  Eg
      ;; quoted-printable-decode-region gets very slow otherwise (Bug#4993).
      (fundamental-mode)
      (erase-buffer)
      (insert data)
      (rmail-mime-show t)
      (rmail-mime-mode)
      (set-buffer-modified-p nil))
    (view-buffer buf)))

(require 'ansi-color)
(defun ansi-colorize-region (&optional start end)
  "ANSI colorize a region"
  (interactive (list (mark) (point)))
  (ansi-color-apply-on-region start end))


