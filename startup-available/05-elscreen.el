(rk-require-packages elscreen)

(rk-bind-keys '(("C-z")))

(use-package elscreen
  :demand t
  :bind
  (:map elscreen-map
        ("z" . elscreen-toggle)
        ("\C-z" . elscreen-toggle))
  :config
  (global-unset-key "\C-z")
  (setq elscreen-display-tab nil))

(defmacro def-elscreen-goto (i)
  `(defun ,(intern (format "elscreen-goto-%d" i)) ()
     (interactive)
     (elscreen-goto ,i)))

(def-elscreen-goto 0)
(def-elscreen-goto 1)
(def-elscreen-goto 2)
(def-elscreen-goto 3)
(def-elscreen-goto 4)
(def-elscreen-goto 5)
(def-elscreen-goto 6)
(def-elscreen-goto 7)
(def-elscreen-goto 8)

(elscreen-start)

(dotimes (i 9)
  (global-set-key (kbd (format "H-%d" i))
                  (intern (format "elscreen-goto-%d" i)))
  (global-set-key (kbd (format "C-z C-%d" i))
                  (intern (format "elscreen-goto-%d" i))))
(global-set-key (kbd "H-n") 'elscreen-next)
(global-set-key (kbd "H-p") 'elscreen-previous)
(global-set-key (kbd "H-b") 'elscreen-find-and-goto-by-buffer)
