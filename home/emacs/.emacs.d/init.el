;; add ~/.emacs.d/lisp to load path
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "evil-rebellion" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'default-frame-alist '(font . "IBM Plex Mono-11"))

;; load files
(require '00-use-package)

(use-package diminish)
(use-package bind-key)
(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-enable-extended-define-key t)
  :config
  (which-key-mode))

(use-package evil
  :config
  (evil-mode 1))

(defvar my-leader-map (make-sparse-keymap)
  "Keymap for \"leader key\" shortcuts.")

(define-key evil-normal-state-map (kbd "SPC") my-leader-map)

; Project settings
(define-key my-leader-map "p" '("project"))
(define-key my-leader-map "pp" 'projectile-switch-project)
(define-key my-leader-map "pf" 'fzf-projectile)

; Window
(define-key my-leader-map "w" '("window"))
(define-key my-leader-map "wh" 'evil-window-left)
(define-key my-leader-map "wl" 'evil-window-right)
(define-key my-leader-map "wj" 'evil-window-down)
(define-key my-leader-map "wk" 'evil-window-up)
(define-key my-leader-map "wd" 'delete-window)
(define-key my-leader-map "wD" 'delete-other-window)
(define-key my-leader-map "ws" 'split-window-below)
(define-key my-leader-map "wv" 'split-window-right)

; File
(define-key my-leader-map "f" '("file"))
(define-key my-leader-map "ff" 'find-file)
(define-key my-leader-map "fw" 'save-buffer)
(define-key my-leader-map "fe" '("edit"))
(defun my-edit-init-file ()
  (interactive)
  (find-file user-init-file))
(define-key my-leader-map "fed" '("init.el" . my-edit-init-file))

; Help
(define-key my-leader-map "h" '("help"))
(define-key my-leader-map "hd" '("describe"))
(define-key my-leader-map "hdb" 'describe-bindings)
(define-key my-leader-map "hdc" 'describe-char)
(define-key my-leader-map "hdf" 'describe-function)
(define-key my-leader-map "hdk" 'describe-key)
(define-key my-leader-map "hdp" 'describe-package)
(define-key my-leader-map "hdt" 'describe-theme)
(define-key my-leader-map "hdv" 'describe-variable)
(define-key my-leader-map "hn" 'view-emacs-news)

;; Git
(define-key my-leader-map "g" '("git"))
(define-key my-leader-map "gb" 'magit-blame)
(define-key my-leader-map "gs" 'magit-status)
(define-key my-leader-map "gS" 'magit-stage-file)
(define-key my-leader-map "gU" 'magit-unstage-file)
(define-key my-leader-map "gf" '("file"))
(define-key my-leader-map "gfh" 'magit-log-buffer-file)
(define-key my-leader-map "gr" 'magit-refresh)
(define-key my-leader-map "gR" 'magit-refresh-all)



; Quit
(define-key my-leader-map "q" '("quit"))
(defun my-quit-emacs ()
  (interactive)
  (save-some-buffers)
  (kill-emacs))
(define-key my-leader-map "qq" '("quit" . my-quit-emacs))
;; Buffer
(define-key my-leader-map "b" '("buffer"))
(define-key my-leader-map "bb" 'ivy-switch-buffer)
(define-key my-leader-map "br" 'counsel-recentf)

;; relative line numbers
(setq-default display-line-numbers 'relative)

(recentf-mode 1)

(use-package
  color-theme-sanityinc-tomorrow
  :config (color-theme-sanityinc-tomorrow-eighties))
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))
(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode)
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit))
(use-package fzf
  :quelpa (fzf :fetcher github :repo "aeruder/fzf.el"))
(use-package projectile-ripgrep)
(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode)
  (defun projectile-find-file (&optional arg)
    (interactive "P")
    (fzf-projectile)))
(use-package counsel-projectile)
(use-package org)
(use-package magit)
(use-package evil-magit)
(use-package vi-tilde-fringe
  :diminish vi-tilde-fringe-mode
  :config
  (vi-tilde-fringe-mode))
(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))
(use-package lsp-mode)
(use-package company
  :diminish company-mode
  :config
  (add-hook 'after-init-hook 'global-company-mode))
(use-package company-lsp)

(use-package
  evil-escape
  :diminish evil-escape-mode
  :config
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.2)
  (evil-escape-mode))
(use-package evil-lisp-state)
(use-package evil-exchange)
(use-package evil-indent-textobject)
(use-package evil-commentary)
(use-package evil-surround)
(use-package evil-easymotion)
(use-package evil-numbers)
(use-package evil-search-highlight-persist)
(use-package evil-org
  :commands evil-org-mode
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
(use-package lispyville
  :commands lispyville-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'lispyville-mode))

;; perl stuff
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("zrperl" . cperl-mode))

(setq cperl-indent-parens-as-block t)
(setq cperl-close-paren-offset -2)
