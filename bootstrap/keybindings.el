(global-unset-key "\M-g")
(global-set-key "\M-g" 'goto-line)
(global-set-key "\r" 'newline-and-indent)
(global-set-key "\C-xn" 'my-next-window)
(global-set-key "\C-xp" 'my-previous-window)
(define-key minibuffer-local-completion-map '[tab] 'minibuffer-complete)
(define-key minibuffer-local-completion-map '[spc] 'minibuffer-complete-word)
(define-key minibuffer-local-must-match-map '[tab] 'minibuffer-complete)
(define-key minibuffer-local-must-match-map '[spc] 'minibuffer-complete-word)
(global-set-key "\C-ct" 'transpose-lines)
(global-set-key "\C-\M-l" 'my-other-buffer)
(global-unset-key "\C-\\")
(global-set-key "\C-\\" 'ext-compile)
(global-set-key "\C-x\C-b" 'ibuffer)
;; (global-set-key "\C-xb" 'iswitchb-buffer)
(global-set-key [C-M-left] 'previous-buffer)
(global-set-key [C-M-right] 'next-buffer)

(global-set-key [?\C-.] 'tags-search)
(global-set-key [?\C-,] 'tags-loop-continue)

(define-key ctl-x-4-map "k" 'other-window-send-keys)

(eval-after-load 'outline
  '(progn
     (define-key outline-minor-mode-map "\C-c[" 'show-entry)
     (define-key outline-minor-mode-map "\C-c]" 'hide-entry)
     (define-key outline-minor-mode-map "\C-c{" 'show-all)
     (define-key outline-minor-mode-map "\C-c}" 'hide-body)))
