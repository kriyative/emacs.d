(load-relative
 (concat (symbol-name (or window-system 'tty)) ".el"))
(blink-cursor-mode -1)
(tool-bar-mode -1)
;; (menu-bar-mode -1)
(setq isearch-lazy-highlight nil)
(set-default 'cursor-in-non-selected-windows nil)
(set-default 'mode-line-in-non-selected-windows nil)
