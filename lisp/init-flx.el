(require-package 'flx)
(require-package 'flx-ido)

(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(ido-vertical-mode 1)

(provide 'init-flx)
