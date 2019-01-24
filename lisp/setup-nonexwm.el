(defun exec! (command)
  (interactive (list (read-shell-command "$ ")))
  (let* ((program-and-args (if (stringp command)
                               (split-string command)
                             command))
         (program (car program-and-args))
         (args (cdr program-and-args))
         (console (get-buffer-create "*Console*"))
         (pt (with-current-buffer console
               (goto-char (point-max))
               (insert "$ "
                       (mapconcat 'identity program-and-args " ")
                       "\n")
               (point)))
         (ret (apply 'call-process program nil console t args))
         (out (with-current-buffer console
                (buffer-substring-no-properties pt (point)))))
    (if (and (numberp ret) (= 0 ret))
        out
      (throw 'exec!-error (list ret out)))))

(defun spawn& (command)
  (interactive (list (read-shell-command "$ ")))
  (let* ((program-and-args (if (stringp command)
                               (split-string command)
                             command))
         (program (car program-and-args))
         (program-name (file-name-nondirectory program))
         (program-buffer (concat " *" program-name))
         (args (cdr program-and-args)))
    (apply 'start-process program-name program-buffer program args)))

(defun setup-elscreen ()
  (global-unset-key "\C-z")
  (setq elscreen-display-tab nil))

(use-package elscreen
  :config (setup-elscreen)
  :bind (:map elscreen-map
              ("z" . elscreen-toggle)
              ("\C-z" . elscreen-toggle)))

(elscreen-start)

(require 'magit-popup)

(defun gnome-screenshot (args)
  (interactive (list (gnome-screenshot-arguments)))
  (exec! (list* "gnome-screenshot" args)))

(magit-define-popup gnome-screenshot-popup
  "Show popup buffer featuring Gnome screenshot commands"
  'exwm-commands
  :switches '((?a "Area"      "--area")
              (?b "Border"    "--include-border")
              (?c "Clipboard" "--clipboard")
              (?w "Window"    "--window"))
  :options  '((?d "Delay" "--delay=" read-number)
              (?e "Border effect" "--border-effect="
                  (lambda (prompt &rest args)
                    (completing-read
                     prompt
                     '("shadow" "border" "vintage" "none")))))
  :actions  '((?x "Execute" gnome-screenshot))
  :default-action 'gnome-screenshot)

(global-set-key (kbd "<print>") 'gnome-screenshot-popup)

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

(dotimes (i 9)
  (global-set-key (kbd (format "H-%d" i))
                  (intern (format "elscreen-goto-%d" i))))
