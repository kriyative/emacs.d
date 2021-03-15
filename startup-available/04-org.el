(rk-el-get-bundles
 org-mime
 org-passwords
 xcezx/blockdiag-mode
 corpix/ob-blockdiag.el
 alf/ob-restclient.el
 org-sync
 org-present)

;;;;;;;;;;;;;;;; org

(defvar org-journal-date-format "%Y-%m-%d %H:%M:%S"
  "Date format string for journal headings.")

(defun org-journal-entry ()
  "Create a new diary entry for today or append to an existing
one."
  (interactive)
  (switch-to-buffer (find-file org-journal-file))
  (widen)
  (let ((today (format-time-string org-journal-date-format)))
    (beginning-of-buffer)
    (next-line)
    (org-insert-heading)
    (insert today)
    (insert "\n\n\n")
    (backward-char)
    (unless (= 2 (current-column))
      (insert "  "))))

(defun rk-org-time-stamp-inactive ()
  (interactive)
  (org-time-stamp-inactive
   (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun rk-org-mode-hook ()
  (visual-line-mode)
  (org-display-inline-images))

(use-package org
  :config
  (unless (fboundp 'org-at-planning-p)
    (defun org-at-planning-p () nil))
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'rk-org-mode-hook)

  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (global-set-key (kbd "C-c j") 'org-journal-entry)
  (define-key org-mode-map "\C-c!" 'rk-org-time-stamp-inactive)
  (define-key org-mode-map (kbd "C-c (") 'show-all)
  (define-key org-mode-map (kbd "C-c )") 'hide-sublevels)
  (define-key org-mode-map '[C-tab] nil)
  (define-key org-mode-map "\M-n" 'rk-next-page)
  (define-key org-mode-map "\M-p" 'rk-prev-page)
  (define-key org-mode-map (kbd "C-c o") 'org-open-at-point)
  (setq org-src-window-setup 'other-window
        org-agenda-window-setup 'other-window
        org-startup-folded t))

(use-package org-compat)

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
  (setq org-export-html-postamble nil
        org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar"
        org-log-done 'time)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((ditaa . t) (shell . t))))

(use-package org-agenda
  :after org
  :config
  (setq org-agenda-include-diary t
        org-agenda-prefix-format (cons
                                  '(agenda . " %i %-16:c%?-12t% s")
                                  (remove-if (lambda (x)
                                               (eq (car x) 'agenda))
                                             org-agenda-prefix-format))))

(use-package org-passwords
  :after org
  :config
  (setq org-passwords-file "~/.pwcrypt.gpg"
        org-passwords-random-words-dictionary "/etc/dictionaries-common/words"))

(use-package org-mime :after org)
(use-package blockdiag-mode)
(use-package ob-blockdiag
  :after (org blockdiag-mode)
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((blockdiag . t))))

(use-package org-present)

(use-package ob-restclient
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
