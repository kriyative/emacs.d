(rk-require-packages
 wgrep
 ag
 diminish
 ;; joaotavora/sly
 )

;;;;;;;;;;;;;;;;

(use-package wgrep)

(use-package ag
  :config
  (setq ag-reuse-buffers t)
  :bind (("C-c g g" . ag)
         ("H-g g" . ag)
         ("C-c g d" . ag-dired)
         ("H-g d" . ag-dired)
         ("C-c g p" . ag-project)
         ("H-g p" . ag-project)
         ("C-c g r" . ag-regexp)
         ("H-g r" . ag-regexp)))

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
  (add-to-list 'Info-directory-list "/usr/local/share/info")
  (add-to-list 'Info-directory-list (expand-file-name "~/share/info")))

(use-package info
  :config
  (set-face-attribute 'info-header-node nil :foreground "black")
  (set-face-attribute 'info-node nil :foreground "black")
  (rk--add-el-get-info-dirs))

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
