(el-get-bundle swiper)

(defun ivy-display-function-popup-window (text)
  (let ((buf (get-buffer-create "*ivy-completions*")))
    (with-displayed-buffer-window
     buf
     nil
     nil
     (with-current-buffer buf
       (insert
        (setq ivy-insert-debug
              (substring text 1)))))))

(use-package swiper
  :diminish (ivy-mode . "")
  :init
  (ivy-mode 1)
  :bind (("C-M-s" . swiper)
         ("\C-c\C-r" . ivy-resume)
         ("M-x"      . counsel-M-x)
         ("\C-hf"    . counsel-describe-function)
         ("\C-hv"    . counsel-describe-variable)
         ("\C-hl"    . counsel-find-library)
         ("<f2>i"    . counsel-info-lookup-symbol)
         ("<f2>u"    . counsel-unicode-char)
         ("\C-cg"    . counsel-git)
         ("\C-cj"    . counsel-git-grep)
         ("\C-ck"    . counsel-ag)
         ("\C-xl"    . counsel-locate)
         ("C-S-o"    . counsel-rhythmbox))
  :bind (:map read-expression-map
              ("\C-r" . counsel-expression-history))
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format ""
        ivy-height 1000
        enable-recursive-minibuffers t
        ivy-display-function  'ivy-display-function-popup-window
        ))

;; (global-unset-key (kbd "\C-c\C-f"))
;; (global-set-key (kbd "\C-c\C-f") 'find-file)

(defun ivy-restore-key-bindings ()
  (global-set-key (kbd "M-x") 'execute-extended-command)
  (global-set-key (kbd "\C-hf") 'describe-function)
  (global-set-key (kbd "\C-hv") 'describe-variable)
  (global-set-key (kbd "\C-hl") 'find-library)
  (global-set-key (kbd "\C-xl") 'count-lines-page))

;; (ivy-restore-key-bindings)
