;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scroll a yearly calendar by month -- in a forwards or backwards direction. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load "calendar"
  '(progn
     (define-key calendar-mode-map "<" 'lawlist-scroll-year-calendar-backward)
     (define-key calendar-mode-map ">" 'lawlist-scroll-year-calendar-forward) ))

(defun year-calendar (&optional month year)
  "Generate a one (1) year calendar that can be scrolled by month
in each direction. This is a modification of:
http://homepage3.nifty.com/oatu/emacs/calendar.html See also:
http://ivan.kanis.fr/caly.el"
  (interactive)
  (require 'calendar)
  (let* ((current (decode-time (current-time)))
         (month (or month 1))
         (year (or year (nth 5 current))))
    (switch-to-buffer (get-buffer-create calendar-buffer))
    (when (not (eq major-mode 'calendar-mode))
      (calendar-mode))
    (setq displayed-month month)
    (setq displayed-year year)
    (setq buffer-read-only nil)
    (erase-buffer)
    ;; horizontal rows
    (calendar-for-loop j from 0 to 3 do
      ;; vertical columns
      (calendar-for-loop i from 0 to 2 do
        (calendar-generate-month
         ;; month
         (cond
          ((> (+ (* j 3) i month) 12)
           (- (+ (* j 3) i month) 12))
          (t
           (+ (* j 3) i month)))
         ;; year
         (cond
          ((> (+ (* j 3) i month) 12)
           (+ year 1))
          (t
           year))
         ;; indentation / spacing between months
         (+ 5 (* 25 i))))
      (goto-char (point-max))
      (insert (make-string (- 10 (count-lines (point-min) (point-max))) ?\n))
      (widen)
      (goto-char (point-max))
      (narrow-to-region (point-max) (point-max)))
    (widen)
    (goto-char (point-min))
    (setq buffer-read-only t)))

(defun lawlist-scroll-year-calendar-forward (&optional arg event)
  "Scroll the yearly calendar by month in a forward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (unless arg (setq arg 1))
  (save-selected-window
    (if (setq event (event-start event)) (select-window (posn-window event)))
    (unless (zerop arg)
      (let* (
             (month displayed-month)
             (year displayed-year))
        (calendar-increment-month month year arg)
        (year-calendar month year)))
    (goto-char (point-min))
    (run-hooks 'calendar-move-hook)))

(defun lawlist-scroll-year-calendar-backward (&optional arg event)
  "Scroll the yearly calendar by month in a backward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (lawlist-scroll-year-calendar-forward (- (or arg 1)) event))
