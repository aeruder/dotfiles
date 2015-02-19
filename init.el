;; add ~/.emacs.d/lisp to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "evil-rebellion" user-emacs-directory))

;; load files

(require 'init-package)
(require 'init-evil)
(require 'init-helm)
(require 'init-ace)
(require 'init-ag)
(require 'init-appearance)
(require 'init-backup)
(require 'init-c-mode)
(require 'init-company)
(require 'init-flx)
(require 'init-git)
(require 'init-linenums)
(require 'init-misc)
(require 'init-org)
(require 'init-projectile)
(require 'init-whitespace)
(require 'init-yasnippet)

;; local settings (optional)
(require 'init-local nil t)
