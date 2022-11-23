;;;;;;;;;;;;;;;; org

(defvar rk--org-journal-date-format "%Y-%m-%d %H:%M:%S"
  "Date format string for journal headings.")

(defun rk-org-journal-entry ()
  "Create a new diary entry for today or append to an existing
one."
  (interactive)
  (unless (eq 'org-mode major-mode)
    (switch-to-buffer (find-file rk--org-journal-file)))
  (widen)
  (let ((today (format-time-string rk--org-journal-date-format)))
    (beginning-of-buffer)
    (org-goto-first-child)
    (org-insert-heading)
    (insert "[" today "] ")
    (save-excursion
      (insert "\n\n\n"))))

(defun rk-org-time-stamp-inactive ()
  (interactive)
  (org-time-stamp-inactive
   (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun rk-org-mode-hook ()
  (visual-line-mode 1)
  (org-display-inline-images))

(use-package org
  :straight t
  :config
  (unless (fboundp 'org-at-planning-p)
    (defun org-at-planning-p () nil))
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'rk-org-mode-hook)

  (setq org-src-window-setup 'other-window
        org-agenda-window-setup 'other-window
        org-startup-folded t
        org-babel-python-command "python3"
        org-log-done 'time
        org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar"
        org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((ditaa . t)
                                 (shell . t)
                                 (plantuml . t)
                                 (dot . t)
                                 (python . t)))

  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c j" . rk-org-journal-entry)

   :map org-mode-map
   ("C-c !" . rk-org-time-stamp-inactive)
   ("C-c (" . show-all)
   ("C-c )" . hide-sublevels)
   ([C-tab] . nil)
   ("\M-n" . rk-next-page)
   ("\M-p" . rk-prev-page)
   ("C-c o" . org-open-at-point)))

(use-package ox-latex
  :after org
  :config
  (add-to-list 'org-latex-classes
               '("letter"
                 "\\documentclass{letter}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package ox-beamer)

(use-package ox-publish
  :after org
  :after org-compat
  :config
  (setq org-export-html-postamble nil))

(use-package org-agenda
  :after org
  :config
  (setq org-agenda-include-diary t
        org-agenda-prefix-format (cons
                                  '(agenda . " %i %-16:c%?-12t% s")
                                  (cl-remove-if (lambda (x)
                                                  (eq (car x) 'agenda))
                                                org-agenda-prefix-format))))

(use-package org-passwords
  :straight t
  :after org
  :config
  (setq org-passwords-file "~/.pwcrypt.gpg"
        org-passwords-random-words-dictionary "/etc/dictionaries-common/words"))

(use-package org-mime :straight t :after org)
(use-package blockdiag-mode :straight t)
(use-package ob-blockdiag
  :straight t
  :after (org blockdiag-mode)
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((blockdiag . t))))

(use-package org-present :straight t)

(use-package ob-restclient
  :straight t
  :after restclient
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((restclient . t))))

;; from:
;; https://emacs.stackexchange.com/questions/17283/is-it-possible-to-get-prettified-symbols-in-org-mode-source-blocks
(defun org-src-font-lock-fontify-block (lang start end)
  "Fontify code block.
     This function is called by emacs automatic fontification, as long
     as `org-src-fontify-natively' is non-nil."
  (let ((lang-mode (org-src--get-lang-mode lang)))
    (when (fboundp lang-mode)
      (let ((string (buffer-substring-no-properties start end))
            (modified (buffer-modified-p))
            (org-buffer (current-buffer)) pos next)
        (remove-text-properties start end '(face nil))
        (with-current-buffer
            (get-buffer-create
             (concat " org-src-fontification:" (symbol-name lang-mode)))
          (delete-region (point-min) (point-max))
          (insert string " ") ;; so there's a final property change
          (unless (eq major-mode lang-mode) (funcall lang-mode))
          ;; Avoid `font-lock-ensure', which does not display fonts in
          ;; source block.
          (font-lock-fontify-buffer)
          (setq pos (point-min))
          (while (setq next (next-single-property-change pos 'face))
            (put-text-property
             (+ start (1- pos)) (1- (+ start next)) 'face
             (get-text-property pos 'face) org-buffer)
            (setq pos next))
          ;; Addition: also copy 'composition info for prettified symbols
          (setq pos (point-min))
          (while (setq next (next-single-property-change pos 'composition))
            (put-text-property
             (+ start (1- pos)) (1- (+ start next)) 'composition
             (get-text-property pos 'composition) org-buffer)
            (setq pos next))
          ;; End addition
          )
        (add-text-properties
         start end
         '(font-lock-fontified t fontified t font-lock-multiline t))
        (set-buffer-modified-p modified)))))

;; (use-package org-roam
;;   :config
;;   (setq org-roam-directory "~/.emacs.d/org-roam/")
;;   (add-hook 'after-init-hook 'org-roam-mode))

(defun rk-org-md-copy-as-markdown (pt mk)
  (interactive "r")
  (save-excursion
    (let ((org-export-show-temporary-export-buffer nil)
          (buf (generate-new-buffer "*rk-org-md*")))
      (unwind-protect
          (org-export-to-buffer 'md buf
            nil nil nil nil nil
            (lambda ()
              (with-current-buffer buf
                (kill-region (point-min) (point-max)))))
        (kill-buffer buf)))))

(provide 'init-org)
