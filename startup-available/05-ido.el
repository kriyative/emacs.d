(defun ido--disable-line-truncation ()
  (set (make-local-variable 'truncate-lines) nil))

(defun ido--define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))

(defun ido--rebind-keys ()
  (global-set-key "\C-x\C-f" 'ido-find-file)

(use-package ido
  :config
  (setq ido-decorations '("\n-> "
                          ""
                          "\n   "
                          "\n   ..."
                          "[" "]"
                          " [No match]"
                          " [Matched]"
                          " [Not readable]"
                          " [Too big]"
                          " [Confirm]")
        ido-enable-flex-matching t
        ido-everywhere t
        )
  (add-hook 'ido-minibuffer-setup-hook 'ido--disable-line-truncation)
  (add-hook 'ido-setup-hook 'ido--define-keys)
  (ido-mode 1))
