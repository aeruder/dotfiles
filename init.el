;; add ~/.emacs.d/lisp to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; load files

(require 'init-package)

(require-packages
  '(ace-jump-mode
    ag
    evil
    evil-matchit
    evil-numbers
    flx
    flx-ido
    ggtags
    gitattributes-mode
    git-commit-mode
    gitconfig-mode
    gitignore-mode
    git-rebase-mode
    helm
    ido-vertical-mode
    magit
    projectile
    rainbow-mode
    whitespace
    yasnippet
    zenburn-theme))

(require 'init-evil)
(require 'init-ace)
(require 'init-ag)
(require 'init-appearance)
(require 'init-backup)
(require 'init-c-mode)
(require 'init-flx)
(require 'init-git)
(require 'init-helm)
(require 'init-linenums)
(require 'init-misc)
(require 'init-org)
(require 'init-projectile)
(require 'init-whitespace)
(require 'init-yasnippet)

;; local settings (optional)
(require 'init-local nil t)
