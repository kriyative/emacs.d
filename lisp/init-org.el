;;;;;;;;;;;;;;;; org

(defvar rk--org-journal-date-format "%Y-%m-%d %a %H:%M"
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
   ("C-c w" . rk-org-link-copy)

   :map org-mode-map
   ("C-c !" . rk-org-time-stamp-inactive)
   ("C-c (" . show-all)
   ("C-c )" . hide-sublevels)
   ([C-tab] . nil)
   ("\M-n" . rk-next-page)
   ("\M-p" . rk-prev-page)
   ("C-c o" . org-open-at-point)))

(use-package ox-latex
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
  :config
  (setq org-export-html-postamble nil))

(use-package org-contrib :ensure t)

;; (use-package ox-confluence :straight t)

(use-package org-agenda
  :config
  (setq org-agenda-include-diary t
        org-agenda-prefix-format (cons
                                  '(agenda . " %i %-16:c%?-12t% s")
                                  (cl-remove-if (lambda (x)
                                                  (eq (car x) 'agenda))
                                                org-agenda-prefix-format))))

(use-package org-mime :ensure t)
(use-package blockdiag-mode :ensure t)
(use-package ob-blockdiag
  :ensure t
  :after (org blockdiag-mode)
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((blockdiag . t))))

(use-package org-present :straight t)

(use-package ob-restclient
  :ensure t
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
	  (org-export-with-toc nil)
          (buf (generate-new-buffer "*rk-org-md*")))
      (unwind-protect
          (org-export-to-buffer 'md buf
            nil nil nil nil nil
            (lambda ()
              (with-current-buffer buf
                (kill-region (point-min) (point-max)))))
        (kill-buffer buf)))))

(defun rk--org-md-src-block (src-block _contents info)
  "Transcode SRC-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((lang (plist-get (cadr src-block) :language)))
    (concat "```" (if lang lang "")
            "\n"
            (org-export-format-code-default example-block info)
            "\n```\n")))

(defun rk--org-md-example-block (orig-fn example-block &rest args)
  (if (eq 'src-block (car example-block))
      (apply 'rk--org-md-src-block example-block args)
    (apply orig-fn example-block args)))

(advice-add 'org-md-example-block :around #'rk--org-md-example-block)

(defun rk-insert-timestamp-comment ()
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (let ((cnt (if (memq major-mode '(emacs-lisp-mode
                                    lisp-mode
                                    scheme-mode
                                    clojure-mode))
                 3
               1)))
    (dotimes (i cnt)
      (insert comment-start))
    (insert " ---------------- "
            (format-time-string "%FT%T%z")
            " ----------------\n\n")))

;; from: https://emacs.stackexchange.com/a/60555
(defun rk-org-link-copy (&optional arg)
  "Extract URL from org-mode link and add it to kill ring."
  (interactive "P")
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
         (type (org-element-property :type link))
         (url (org-element-property :path link))
         (url (concat type ":" url)))
    (kill-new url)
    (message (concat "Copied URL: " (replace-regexp-in-string "%" "%%" url)))))

(provide 'init-org)
