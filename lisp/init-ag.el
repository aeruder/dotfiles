(require-package 'ag)

(add-hook 'ag-mode-hook
          (lambda ()
            (define-key ag-mode-map "j" 'evil-next-line)
            (define-key ag-mode-map "k" 'evil-previous-line)
            (define-key ag-mode-map "n" 'evil-ex-search-next)
            (define-key ag-mode-map "N" 'evil-ex-search-previous)))
(provide 'init-ag)
