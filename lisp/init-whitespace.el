(require-package 'whitespace)

(setq whitespace-line-column 80)
(setq whitespace-style '(face tabs trailing lines-tail space-before-tab empty))
(global-whitespace-mode t)
(set-face-background 'whitespace-line "#eee8d5")
(set-face-foreground 'whitespace-line "#d33682")
(set-face-background 'whitespace-tab "#eee8d5")
(set-face-foreground 'whitespace-tab "#eee8d5")
; (setq whitespace-action '(report-on-bogus))


(provide 'init-whitespace)
