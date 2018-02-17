(use-package swiper
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t
          enable-recursive-minibuffers t))
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
              ("\C-r" . counsel-expression-history)))

;; (global-unset-key (kbd "\C-c\C-f"))
;; (global-set-key (kbd "\C-c\C-f") 'find-file)
