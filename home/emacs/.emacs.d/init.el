;; add ~/.emacs.d/lisp to load path
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "evil-rebellion" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'default-frame-alist '(font . "IBM Plex Mono-11"))

;; load files
(require '00-use-package)

(use-package cl)
(use-package dash)
(use-package diminish)
(use-package bind-key)
(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-enable-extended-define-key t)
  :config
  (which-key-mode))

(setq evil-want-integration nil)
(use-package evil
  :config
  (evil-mode 1))

(use-package general)

(defvar my-leader-map (make-sparse-keymap)
  "Keymap for \"leader key\" shortcuts.")

(defun aeruder/edit-init-file ()
  (interactive)
  (find-file user-init-file))

(defun aeruder/ls-dotfiles ()
  (interactive)
  (fzf/start (expand-file-name "~/.dotfiles")))

(defun aeruder/align= (start end)
  (interactive "r")
  (align-regexp start end "\\(\\s-*\\)=" 1 1 t))

(defun aeruder/quit ()
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defun aeruder/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
	(delq (current-buffer)
	      (-select (lambda (b)
			 (or (buffer-file-name b)
			     (eq 'dired-mode (buffer-local-value 'major-mode b))))
		       (buffer-list)))))

(defun aeruder/kill-all-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
	(-select (lambda (b)
		   (or (buffer-file-name b)
		       (eq 'dired-mode (buffer-local-value 'major-mode b))))
		 (buffer-list))))

(defun spacemacs/frame-killer ()
  "Kill server buffer and hide the main Emacs window"
  (interactive)
  (condition-case-unless-debug nil
      (delete-frame nil 1)
    (error
     (make-frame-invisible nil 1))))

(defun aeruder/edit-this-dir ()
  (interactive)
  (if (eq major-mode 'dired-mode)
      (find-file "..")
    (find-file ".")))

(defun aeruder/git-file-path (remote line)
  (let* ((relpath (aeruder/copy-file-path t line)))))
(defun aeruder/copy-git-file-path (remote line)
  (interactive)
  (kill-new (aeruder/git-file-path remote line) t))
(defun aeruder/file-path (relative)
  (if relative
      (file-relative-name (buffer-file-name) (projectile-project-root))
    (buffer-file-name)))
(defun aeruder/copy-file-path (relative)
  (interactive)
  (kill-new (aeruder/file-path relative) t))
(defun aeruder/copy-file-path-rel ()
  (interactive)
  (aeruder/copy-file-path t))
(defun aeruder/copy-file-path-abs ()
  (interactive)
  (aeruder/copy-file-path nil))

(general-override-mode)
(general-define-key
 :states '(normal visual motion)
 :keymaps 'override
 "-" 'aeruder/edit-this-dir)
(general-define-key
 "C-c SPC" '(nil :which-key "custom")
 "C-c SPC x" '(nil :which-key "text")
 "C-c SPC x y" 'company-yasnippet)
(general-define-key
 :prefix "SPC"
 :states '(normal visual motion)
 :keymaps 'override
 "" nil

					; Buffer
 "b" '(nil :which-key "buffer")
 "b b" 'ivy-switch-buffer
 "b r" 'counsel-recentf
 "b d" 'kill-current-buffer
 "b D" 'aeruder/kill-all-buffers
 "b m" 'aeruder/kill-other-buffers

					; File
 "f" '(nil :which-key "file")
 "f f" 'find-file
 "f w" 'save-buffer
 "f e" '(nil :which-key "edit")
 "f e d" 'aeruder/edit-init-file
 "f e D" 'aeruder/ls-dotfiles
 "f s" 'server-edit
 "f y" '(nil :which-key "yank")
 "f y y" 'aeruder/copy-file-path-rel
 "f y Y" 'aeruder/copy-file-path-abs
					; Git
 "g" '(nil :which-key "git")
 "g R" 'magit-refresh-all
 "g f" 'magit-file-popup
 "g F" 'magit-fetch-popup
 "g b" 'magit-blame
 "g g" 'magit
 "g h" 'magit-reflog
 "g l" '(nil :which-key "log")
 "g l A" 'magit-log-author
 "g l R" 'magit-reflog
 "g l a" 'magit-log-all
 "g l f" 'magit-log-buffer-file
 "g l r" 'magit-reflog-current
 "g l x" 'magit-rebase-interactive
 "g r" 'magit-refresh
 "g s" 'magit-status

					; Help
 "h" '(nil :which-key "help")
 "h d" '(nil :which-key "describe")
 "h d b" 'describe-bindings
 "h d c" 'describe-char
 "h d f" 'describe-function
 "h d k" 'describe-key
 "h d p" 'describe-package
 "h d t" 'describe-theme
 "h d v" 'describe-variable
 "h n" 'view-emacs-news

					; Lisp
 "l" '(nil :which-key "lisp")
 "l e" 'eval-last-sexp
 "l f" 'lispyville-prettify
 "l x" 'pp-eval-expression
					; Project settings
 "p" '(nil :which-key "project")
 "p p" 'projectile-switch-project
 "p f" 'fzf-projectile
 "p s" 'projectile-ripgrep
					; Quit
 "q" '(nil :which-key "quit")
 "q q" 'spacemacs/frame-killer
 "q Q" 'aeruder/quit

					; Window
 "w" '(nil :which-key "window")
 "w =" 'balance-windows
 "w h" 'evil-window-left
 "w l" 'evil-window-right
 "w j" 'evil-window-down
 "w k" 'evil-window-up
 "w d" 'delete-window
 "w D" 'delete-other-window
 "w s" 'split-window-below
 "w v" 'split-window-right



					; Text
 "x" '(nil :which-key "text")
 "x +" 'evil/inc-at-pt
 "x -" 'evil/dec-at-pt
 "x a" '(nil :which-key "align")
 "x a =" 'aeruder/align=)


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
  (global-vi-tilde-fringe-mode))
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
  (add-to-list 'evil-escape-excluded-major-modes 'magit-status-mode)
  (evil-escape-mode))
(use-package evil-lisp-state)
(use-package evil-exchange)
(use-package evil-indent-textobject)
(use-package evil-commentary)
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))
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
(use-package lispy
  :commands lispy-mode
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
  (add-hook 'lisp-mode-hook (lambda () (lispy-mode 1))))
(use-package lispyville
  :commands lispyville-mode
  :init
  (add-hook 'lispy-mode-hook #'lispyville-mode)
  (lispyville-set-key-theme '(operators c-w prettify additional-movement slurp/barf-cp wrap)))
(use-package evil-collection
  :config
  (delq 'diff-mode evil-collection-mode-list)
  (evil-collection-init))
(use-package yaml-mode)
(use-package yasnippet
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets.private" user-emacs-directory) t)
  (yas-global-mode 1))

(evil-set-initial-state 'term-mode 'emacs)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)))

(use-package cperl-mode
  :quelpa (fzf :fetcher github :repo "jrockway/cperl-mode")
  :init
  (setq cperl-indent-parens-as-block t)
  (setq cperl-close-paren-offset -2)
  (setq cperl-under-as-char t)
  (setq cperl-hairy nil)
  (setq cperl-font-lock t)
  (setq cperl-electric-keywords nil))

;; perl stuff
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("zrperl" . cperl-mode))

;; Put backup files neatly away                                                 
(let ((backup-dir "~/.emacs.d/backups")
      (auto-saves-dir "~/.emacs.d/backups"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
	auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
	auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
	tramp-backup-directory-alist `((".*" . ,backup-dir))
	tramp-auto-save-directory auto-saves-dir))

(setq backup-by-copying t	     ; Don't delink hardlinks
      delete-old-versions t	     ; Clean up the backups
      version-control t		     ; Use version numbers on backups,
      kept-new-versions 5	     ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too    

(tool-bar-mode -1)
(menu-bar-mode -1)

(server-start)