
;;;
;;; Ace jump
;;;
(require-package 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-char-mode)
(define-key evil-visual-state-map (kbd "SPC") 'ace-jump-char-mode)
(define-key evil-motion-state-map (kbd "SPC") 'evil-ace-jump-char-mode)
(setq ace-jump-mode-scope 'window)
(provide 'init-ace)
