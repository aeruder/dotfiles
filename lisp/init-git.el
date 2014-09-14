(setq auto-mode-alist (delete '("/git-rebase-todo\\'" . git-rebase-mode)
                              auto-mode-alist))

(provide 'init-git)
