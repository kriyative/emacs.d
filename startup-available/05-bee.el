(add-to-list 'load-path
             (format "/usr/local/share/emacs/%s/site-lisp/bigloo"
                     emacs-version))

(autoload 'bdb "bdb" "bdb mode" t)
(autoload 'bee-mode "bee-mode" "bee mode" t)

(setq auto-mode-alist
      (append '(("\\.bgl$" . bee-mode)
                ("\\.bee$" . bee-mode))
              auto-mode-alist))
