(require-package 'gitattributes-mode)
(require-package 'git-commit-mode)
(require-package 'gitconfig-mode)
(require-package 'gitignore-mode)
(require-package 'git-rebase-mode)
(require-package 'magit)

(setq auto-mode-alist (delete '("/git-rebase-todo\\'" . git-rebase-mode)
                              auto-mode-alist))

(provide 'init-git)
