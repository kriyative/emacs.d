;;;;;;;;;;;;;;;; el-get initialization ;;;;;;;;;;;;;;;;

(rk-el-get-bundles
 alert
 buffer-move
 (emacs-fun :url "https://github.com/kriyative/emacs-fun.git"
            :features (efun-base efun-cmds))
 popup
 use-package)

;;;;;;;;;;;;;;;; packages ;;;;;;;;;;;;;;;;

(use-package ibuffer
  :config
  (setq ibuffer-expert t)
  (global-set-key "\C-x\C-b" 'ibuffer))

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
  (setq dired-listing-switches "-alh"))

(use-package dired-x
  :after dired
  :config
  (set-default 'dired-omit-mode t)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
  :bind (:map dired-mode-map
              ("k" . dired-kill-subdir)
              (">" . dired-omit-mode)
              ([C-return] . dired-open-file)))

(use-package efun-cmds)

(defun fortune-computers ()
  (interactive)
  (fortune (concat fortune-dir "/computers")))

(use-package fortune
  :bind (:map user-commands-prefix-map
              ("ff" . fortune)
              ("fc" . fortune-computers))
  :config
  (setq fortune-dir "/usr/share/games/fortunes"
        fortune-file "/usr/share/games/fortunes/fortunes"))

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
      completion-cycle-threshold nil
      read-buffer-completion-ignore-case nil
      completion-ignore-case nil
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

(add-to-list 'display-buffer-alist
             '("\\*Completions\\*" vertical-display-completions))

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

(defun confirm-exit ()
  (y-or-n-p "Exit Emacs, Are you sure? "))

(add-to-list 'kill-emacs-query-functions 'confirm-exit)

(defun post-startup-hook ()
  (when (fboundp 'global-auto-complete-mode)
    (global-auto-complete-mode -1)))

(add-hook 'emacs-startup-hook 'post-startup-hook)

;;;;;;;;;;;;;;;; keys ;;;;;;;;;;;;;;;;

(global-unset-key "\C-z")
(global-unset-key "\M-g")
(global-set-key "\M-g" 'goto-line)
(global-set-key "\r" 'newline-and-indent)
(global-set-key "\C-xn" 'rk-next-window)
(global-set-key "\C-xp" 'rk-previous-window)
(define-key minibuffer-local-completion-map '[tab] 'minibuffer-complete)
(define-key minibuffer-local-completion-map '[spc] 'minibuffer-complete-word)
(define-key minibuffer-local-must-match-map '[tab] 'minibuffer-complete)
(define-key minibuffer-local-must-match-map '[spc] 'minibuffer-complete-word)
(global-set-key "\C-ct" 'transpose-lines)
(global-set-key "\C-\M-l" 'rk-other-buffer)
(global-unset-key "\C-\\")
(global-set-key "\C-\\" 'compile)
;; (global-set-key "\C-xb" 'iswitchb-buffer)
;; (global-set-key "\C-xb" 'switch-to-buffer)
(global-set-key [C-M-left] 'previous-buffer)
(global-set-key [C-M-right] 'next-buffer)
(global-set-key [?\C-.] 'tags-search)
(global-set-key [?\C-,] 'tags-loop-continue)
(global-set-key "\C-x\C-f" 'x-find-file)
(global-set-key (kbd "<f7>") 'next-error)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

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

;; (global-set-key (kbd "<f2>") 'save-buffer)
;; (global-set-key (kbd "<f3>") 'find-file)
;; (global-set-key (kbd "<f4>") 'switch-to-buffer)
;; (global-set-key (kbd "<f5>") 'ibuffer-other-window)
;; (global-set-key (kbd "<f6>") 'completion-at-point)
(global-set-key (kbd "M-i") 'completion-at-point)
(global-set-key (kbd "M-j") 'jump-to-register)
(global-set-key (kbd "M-k") 'kill-sentence)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-a") 'beginning-of-line)
(global-set-key (kbd "M-e") 'end-of-line)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "H-SPC") 'backward-delete-char-untabify)

