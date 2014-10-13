(require-package 'whitespace)

(setq whitespace-line-column 80)
(setq whitespace-style
  '(empty face lines-tail space-before-tab tabs tab-mark trailing))
(global-whitespace-mode t)

(provide 'init-whitespace)
