(yas-global-mode 1)
;;Allow quick manual triggering of snippets
(define-key evil-normal-state-map (kbd "C-c s") 'yas-insert-snippet)
(define-key evil-insert-state-map (kbd "C-c s") 'yas-insert-snippet)

(global-linum-mode t)
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

(provide 'init-yasnippet)
