(require-package 'whitespace)

(setq whitespace-line-column 80)
(setq whitespace-style
  '(empty face lines-tail space-before-tab tabs tab-mark trailing))
(global-whitespace-mode t)

(setq whitespace-empty '(t (:foreground "black" :background "#800")))
(setq whitespace-line '(t (:foreground "black" :background "#aa0")))
(setq whitespace-space-before-tab '(t (:foreground "black" :background "#800")))
(setq whitespace-tab '(t (:foreground "#aa0" :background "#333")))
(setq whitespace-trailing '(t (:foreground "#800" :background "#0aa")))

(provide 'init-whitespace)
