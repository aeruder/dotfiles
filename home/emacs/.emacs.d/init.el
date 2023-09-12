;; add ~/.emacs.d/lisp to load path
;; (toggle-debug-on-quit)
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "evil-rebellion" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
(setq package-native-compile t)
(setq warning-suppress-types '((comp)))

(add-to-list 'default-frame-alist '(font . "IBM Plex Mono-11"))

;; load files
(require '00-use-package)

(use-package dash)
(use-package diminish)
(use-package bind-key)
(use-package monitor)
(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-enable-extended-define-key t)
  :config
  (which-key-mode))

(use-package vundo
  :config
  ;; Use `HJKL` VIM-like motion, also Home/End to jump around.
  (define-key vundo-mode-map (kbd "l") #'vundo-forward)
  (define-key vundo-mode-map (kbd "<right>") #'vundo-forward)
  (define-key vundo-mode-map (kbd "h") #'vundo-backward)
  (define-key vundo-mode-map (kbd "<left>") #'vundo-backward)
  (define-key vundo-mode-map (kbd "j") #'vundo-next)
  (define-key vundo-mode-map (kbd "<down>") #'vundo-next)
  (define-key vundo-mode-map (kbd "k") #'vundo-previous)
  (define-key vundo-mode-map (kbd "<up>") #'vundo-previous)
  (define-key vundo-mode-map (kbd "<home>") #'vundo-stem-root)
  (define-key vundo-mode-map (kbd "<end>") #'vundo-stem-end)
  (define-key vundo-mode-map (kbd "q") #'vundo-quit)
  (define-key vundo-mode-map (kbd "C-g") #'vundo-quit)
  (define-key vundo-mode-map (kbd "RET") #'vundo-confirm))
(use-package evil
  :init
  (setq evil-want-C-i-jump nil)         ; necessary for terminal because C-i is tab
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(use-package general)

(defvar my-leader-map (make-sparse-keymap)
  "Keymap for \"leader key\" shortcuts.")

;; (defun spacemacs//helm-hide-minibuffer-maybe ()
;;   "Hide minibuffer in Helm session if we use the header line as input field."
;;   (when (with-helm-buffer helm-echo-input-in-header-line)
;;     (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
;;       (overlay-put ov 'window (selected-window))
;;       (overlay-put ov 'face
;;                    (let ((bg-color (face-background 'default nil)))
;;                      `(:background ,bg-color :foreground ,bg-color)))
;;       (setq-local cursor-type nil))))

(defun aeruder/edit-init-file ()
  (interactive)
  (find-file user-init-file))

(defun aeruder/edit-todo-file ()
  (interactive)
  (find-file (expand-file-name "~/todo.org")))

(defun aeruder/ls-dotfiles ()
  (interactive)
  (helm-fzf (expand-file-name "~/.dotfiles")))

(defun aeruder/fzf-projectile ()
  (interactive)
  (helm-fzf (projectile-project-root)))

(defun aeruder/fzf-same-projectile ()
  (interactive)
  (helm-fzf-same (projectile-project-root)))

(defun aeruder/ripgrep-projectile ()
  (interactive)
  (helm-ripgrep (projectile-project-root)))

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
  (print "starting")
  (let* ((xterm-set-window-title nil)
          (list (delq (current-buffer) (buffer-list))
            ))
  (dolist (element list)
      (print element)
      (if (and (not (buffer-modified-p element))
               (not (get-buffer-window element t)))
        (progn
          ;; (print element)
          (ignore-errors
            (kill-buffer element))
            ))
      )))

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

(defun aeruder/kill-new (str replace)
  (message "Copied '%s'" str)
  (kill-new str replace))
(defun aeruder/file-path (relative)
  (if relative
      (file-relative-name (buffer-file-name) (magit-toplevel))
    (buffer-file-name)))
(defun aeruder/copy-file-path-rel ()
  (interactive)
  (aeruder/kill-new (aeruder/file-path t) t))
(defun aeruder/copy-file-path-abs ()
  (interactive)
  (aeruder/kill-new (aeruder/file-path nil) t))
(defun aeruder/make-file-executable ()
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (set-file-modes file 493))))

(setq aeruder/scratch-dir
  (cond
    ((file-exists-p (expand-file-name "~/Projects/")) (expand-file-name "~/Projects/scratch"))
    ((file-exists-p (expand-file-name "~/Project/")) (expand-file-name "~/Project/scratch"))
    ((file-exists-p (expand-file-name "~/proj/")) (expand-file-name "~/proj/scratch"))
    ((file-exists-p (expand-file-name "~/Documents/")) (expand-file-name "~/Documents/Scratch"))
    (t (expand-file-name "~/scratch"))))

(defun aeruder/new-scratch-file (filename)
  "Create new scratch dir"
  (interactive "sEnter scratch file name: ")
  (let* ((ext (or (file-name-extension filename) "txt"))
         (timedir (format-time-string "%Y-%m"))
         (scratch (concat (file-name-as-directory aeruder/scratch-dir) timedir))
         (bn (file-name-sans-extension filename))
         (ver -1)
         (done nil))
    (unless (file-exists-p (file-name-as-directory scratch)) (mkdir scratch))
    (while (and (not done) (< ver 1000))
      (let ((testfile (cond ((= ver -1) (format "%s%s.%s" (file-name-as-directory scratch) bn ext))
                            (t (format "%s%s.%03d.%s" (file-name-as-directory scratch) bn ver ext)))))
        (unless (file-exists-p testfile)
          (find-file testfile)
          (setq done t)))
      (setq ver (1+ ver)))))

(defun aeruder/magit-remote-url (remote)
  (magit-git-str "remote" "get-url" remote))

(defun aeruder/magit-normalize-ssh-host (host)
  (let* ((ssh-command (format "/usr/bin/env ssh -TG %s" host))
         (config-output (shell-command-to-string ssh-command))
         (lines (split-string config-output "\n"))
         (hostname-config (seq-find (lambda (x) (string-match-p "^hostname " x)) lines (format "hostname %s" host)))
         (hostname (replace-regexp-in-string "^hostname " "" hostname-config)))
    hostname))

(defun aeruder/magit-remote-host (remote)
  (save-match-data
    (let* ((remote-url (aeruder/magit-remote-url remote)))
      (if (string-match "^[a-z]+://\\([^/]+\\)" remote-url)
          (match-string-no-properties 1 remote-url)
        (if (string-match "\\([a-zA-Z0-9_.-]+\\):" remote-url)
            (aeruder/magit-normalize-ssh-host (match-string-no-properties 1 remote-url))
          nil)))))

(defun aeruder/magit-remote-repo (remote)
  (save-match-data
    (let* ((remote-url (aeruder/magit-remote-url remote)))
      (if (string-match "^[a-z]+://\\([^/]+\\)/\\(.*?\\)\\(\\.git\\)*$" remote-url)
          (match-string-no-properties 2 remote-url)
        (if (string-match "\\([a-zA-Z0-9_.-]+\\):\\(.*?\\)\\(\\.git\\)*$" remote-url)
            (match-string-no-properties 2 remote-url)
          nil)))))

(defun aeruder/magit-browse-url (remote commit path line)
  (let* (
         (remote-host (aeruder/magit-remote-host remote))
         (remote-repo (aeruder/magit-remote-repo remote))
         (url (concat "https://" remote-host "/" remote-repo "/blob/" commit "/" path)))
    (if line
        (concat url "#L" (number-to-string line))
      url)))

(defun aeruder/magit-get-upstream-branch-minus-remote (&optional branch)
  (save-match-data
    (let* (
           (remote (magit-get-upstream-remote branch))
           (full-branch (magit-get-upstream-branch branch)))
      (if (and remote full-branch (string-match (format "^%s/\\(.*\\)" (regexp-quote remote)) full-branch))
          (match-string-no-properties 1 full-branch)
        (if full-branch full-branch (magit-rev-parse "HEAD"))))))

(defun aeruder/copy-url-path ()
  (interactive)
  (aeruder/kill-new (aeruder/magit-browse-url
                     (or (magit-get-upstream-remote) (magit-get-current-remote))
                     (aeruder/magit-get-upstream-branch-minus-remote)
                     (aeruder/file-path t)
                     nil) t))

(defun aeruder/copy-url-path-line ()
  (interactive)
  (aeruder/kill-new (aeruder/magit-browse-url
                     (or (magit-get-upstream-remote) (magit-get-current-remote))
                     (aeruder/magit-get-upstream-branch-minus-remote)
                     (aeruder/file-path t)
                     (line-number-at-pos)) t))

(defun aeruder/reformat-sql ()
  (interactive)
  (let (string)
    (unless (mark)
      (error "No region selected"))
    (shell-command-on-region (region-beginning) (region-end) "sql-formatter-cli" t t)))

(defun aeruder/reset-buffer-major-mode ()
  (interactive)
  (let ((this-major-mode (buffer-local-value 'major-mode (current-buffer))))
    (when this-major-mode
      (eval 'this-major-mode))))

;; mercilessly stolen
(defun rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (message "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)))))))

;; and again stolen
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (delete-file filename)
      (message "Deleted file %s" filename)
      (kill-buffer))))

(defun pbcopy ()
  (interactive)
  (call-process-region (point) (mark) "pbcopy")
  (setq deactivate-mark t))

(defun pbpaste ()
  (interactive)
  (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

(defun pbcut ()
  (interactive)
  (pbcopy)
  (delete-region (region-beginning) (region-end)))

(xterm-mouse-mode 1)
(setq xterm-set-window-title t)

(setq last-paste-to-osx nil)

(defun copy-from-osx ()
  (with-temp-buffer
    (cd temporary-file-directory)
    (if (executable-find "pbpaste")
        (let ((copied-text (shell-command-to-string "pbpaste")))
          (unless (string= copied-text last-paste-to-osx)
            copied-text))
      (if (executable-find "wl-paste")
          (let ((copied-text (shell-command-to-string "wl-paste -n")))
            (unless (string= copied-text last-paste-to-osx)
              copied-text))
        (let ((copied-text (shell-command-to-string "xsel -o")))
          (unless (string= copied-text last-paste-to-osx)
            copied-text))))))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (if (executable-find "pbcopy")
        (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
          (process-send-string proc text)
          (process-send-eof proc))
      (if (executable-find "wl-copy")
          (let ((proc (start-process "wl-copy" "*Messages*" "wl-copy")))
            (process-send-string proc text)
            (process-send-eof proc))
        (let ((proc (start-process "xsel-primary" "*Messages*" "xsel" "-i" "-p")))
          (process-send-string proc text)
          (process-send-eof proc))
        (let ((proc (start-process "xsel-secondary" "*Messages*" "xsel" "-i" "-s")))
          (process-send-string proc text)
          (process-send-eof proc))
        (let ((proc (start-process "xsel-clipboard" "*Messages*" "xsel" "-i" "-b")))
          (process-send-string proc text)
          (process-send-eof proc)))
      (setq last-paste-to-osx text))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

(general-override-mode)
(general-define-key
  :states '(normal visual motion)
  :keymaps 'override
  "-" 'aeruder/edit-this-dir
  "]n" 'diff-hl-next-hunk
  "[n" 'diff-hl-previous-hunk
  "]q" 'compilation-next-error
  "[q" 'compilation-previous-error
  "g c" 'evil-commentary-line
  "z o" 'origami-open-node
  "z O" 'origami-open-node-recursively
  "z c" 'origami-close-node
  "z C" 'origami-close-node-recursively
  "z R" 'origami-open-all-nodes
  "z M" 'origami-close-all-nodes)

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
  "b b" 'helm-buffers-list
  "b r" 'helm-recentf
  "b d" 'kill-current-buffer
  "b D" 'aeruder/kill-all-buffers
  "b m" 'aeruder/kill-other-buffers
  "b x" 'aeruder/reset-buffer-major-mode

                                        ; File
  "f" '(nil :which-key "file")
  "f f" 'helm-find-files
  "f R" 'read-only-mode
  "f w" 'save-buffer
  "f b" '(nil :which-key "bookmark")
  "f b b" 'bookmark-jump
  "f b s" 'bookmark-save
  "f b l" 'bookmark-bmenu-list
  "f e" '(nil :which-key "edit")
  "f e d" 'aeruder/edit-init-file
  "f e D" 'aeruder/ls-dotfiles
  "f e t" 'aeruder/edit-todo-file
  "f n" '(nil :which-key "new")
  "f n s" 'aeruder/new-scratch-file
  "f s" 'server-edit
  "f x" 'aeruder/make-file-executable
  "f y" '(nil :which-key "yank")
  "f y y" 'aeruder/copy-file-path-rel
  "f y Y" 'aeruder/copy-file-path-abs
  "f y g" 'aeruder/copy-url-path
  "f y G" 'aeruder/copy-url-path-line
                                        ; Git
  "g" '(nil :which-key "git")
  "g R" 'magit-refresh-all
  "g f" 'magit-file-dispatch
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
  "h a" 'helm-apropos
  "h b" 'helm-descbinds
  "h d" '(nil :which-key "describe")
  "h d b" 'describe-bindings
  "h d c" 'describe-char
  "h d f" 'describe-function
  "h d k" 'describe-key
  "h d p" 'describe-package
  "h d t" 'describe-theme
  "h d v" 'describe-variable
  "h n" 'view-emacs-news

                                        ; Jump
  "j" '(nil :which-key "jump")
  "j l" 'helm-swoop
  "j h" 'helm-resume

                                        ; Lisp
  "l" '(nil :which-key "lisp")
  "l e" 'eval-last-sexp
  "l f" 'lispyville-prettify
  "l x" 'pp-eval-expression

  "o" '(nil :which-key "org")
  "o a" 'org-agenda
  "o c" 'org-capture
                                        ; Project settings
  "p" '(nil :which-key "project")
  "p p" 'projectile-switch-project
  "p f" 'aeruder/fzf-projectile
  "p F" 'aeruder/fzf-same-projectile
  "p s" 'projectile-ripgrep
  "p S" 'aeruder/ripgrep-projectile
  "p t" 'helm-gtags-find-tag
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
  "x +" 'evil-numbers/inc-at-pt
  "x -" 'evil-numbers/dec-at-pt
  "x a" '(nil :which-key "align")
  "x a =" 'aeruder/align=
  "x a S" 'aeruder/reformat-sql)
(global-set-key (kbd "M-x") 'helm-M-x)

;; relative line numbers
(setq-default display-line-numbers 'relative)

(recentf-mode 1)

;; (use-package doom-themes
;;   :config
  ;; Global settings (defaults)
  ;; (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
  ;;       doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-dracula t)
  ;; (load-theme 'doom-gruvbox t)
  ;; (load-theme 'doom-manegarm t)

  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  ;; (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  ;; (doom-themes-org-config))
;; (use-package green-is-the-new-black-theme)
;; (use-package base16-theme
;;   :config
;;   (setq base16-theme-256-color-source "colors")
;;   (load-theme 'base16-greenscreen))
;; (use-package ample-theme
;;   :config (load-theme 'ample-flat t))
;; (use-package dracula-theme
;;   :config (load-theme 'dracula t))
;; (use-package soothe-theme
;;   :config (load-theme 'soothe))
;; (use-package nova-theme
;;   :config (load-theme 'nova t))
;; (use-package alect-themes
;;   :config (load-theme 'alect-light-alt t))
;; (use-package gruvbox-theme
;;   :config (load-theme 'gruvbox-dark-soft t))
;; (use-package
;;   borland-blue-theme
;;   :config (load-theme 'borland-blue t))
(use-package
  birds-of-paradise-plus-theme
  :config (load-theme 'birds-of-paradise-plus t))
;; (use-package ivy
;;   :diminish ivy-mode
;;   :config
;;   (ivy-mode)
;;   (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit))
(setenv "FZF_DEFAULT_COMMAND" "rg --files --no-ignore --hidden -g !.git -g !node_modules")
(use-package ripgrep
  :config
  (setq ripgrep-arguments (split-string "--hidden --no-ignore --max-columns 400 -g !.git -g !node_modules")))
(use-package projectile-ripgrep)
(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'helm)
  :config
  (projectile-mode)
  (defun projectile-find-file (&optional arg)
    (interactive "P")
    (aeruder/fzf-projectile)))
(use-package counsel-projectile)
(use-package org)
;; (use-package libgit
;;   :quelpa (libgit :fetcher github :repo "magit/libegit2"))
(use-package with-editor
  :quelpa (with-editor :fetcher github :repo "magit/with-editor"
                 :files ("lisp/*.el")))
(use-package transient)
(use-package magit)
;; (use-package evil-magit)
(use-package vi-tilde-fringe
  :diminish vi-tilde-fringe-mode
  :config
  (global-vi-tilde-fringe-mode))
;; (use-package editorconfig
;;   :diminish editorconfig-mode
;;   :config
;;   (editorconfig-mode 1))

;; (use-package lsp-mode
;;   :commands lsp
;;   :diminish lsp-mode
;;   :config
;;   (setq lsp-enable-file-watchers nil)
;;   :hook
;;   (elixir-mode . lsp))

;; (use-package lsp-ui
;;   :config
;;   (setq lsp-ui-doc-enable t
;;         lsp-ui-peek-enable t
;;         lsp-ui-sideline-enable t
;;         lsp-ui-imenu-enable t
;;         lsp-ui-flycheck-enable t)
;;   :init)
(use-package company
  :diminish company-mode
  :config
  (setq company-tooltip-align-annotations t)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package evil-escape
  :diminish evil-escape-mode
  :config
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.2)
  (add-to-list 'evil-escape-excluded-major-modes 'magit-status-mode)
  (add-to-list 'evil-escape-excluded-major-modes 'magit-revision-mode)
  (add-to-list 'evil-escape-excluded-major-modes 'magit-diff-mode)
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
;; (use-package evil-search-highlight-persist)
(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
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
  (lispyville-set-key-theme '(operators c-w prettify additional-movement slurp/barf-cp)))
(use-package evil-collection
  :config
  (delq 'diff-mode evil-collection-mode-list)
  (delq 'outline evil-collection-mode-list)
  (evil-collection-init))
(use-package yaml-mode)
(use-package yasnippet
  :config
  (setq yas-indent-line 'fixed)
  ;; (if (exists
  (let ((privsnip (expand-file-name "snippets.private" user-emacs-directory))) (if (file-directory-p privsnip) (add-to-list 'yas-snippet-dirs (expand-file-name "snippets.private" user-emacs-directory) t)))
  (yas-global-mode 1))

(evil-set-initial-state 'term-mode 'emacs)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)))

(setq cperl-indent-parens-as-block t)
(setq cperl-close-paren-offset -2)
(setq cperl-under-as-char t)
(setq cperl-hairy nil)
(setq cperl-font-lock t)
(setq cperl-electric-keywords nil)

(add-hook 'python-mode-hook (lambda () (editorconfig-mode 0)))

;; (use-package cperl-mode
;;   :quelpa (cperl-mode :fetcher github :repo "aeruder/cperl-mode")
;;   :init
;;   (setq cperl-indent-parens-as-block t)
;;   (setq cperl-close-paren-offset -2)
;;   (setq cperl-under-as-char t)
;;   (setq cperl-hairy nil)
;;   (setq cperl-font-lock t)
;;   (setq cperl-electric-keywords nil))

(use-package raku-mode)
(use-package typescript-mode
  :commands typescript-mode
  :config
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

 ;; GO SETUP
(use-package go-mode)
(use-package company-go)
;; (use-package flymake-go)
(use-package go-guru)
(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save t t)
  (setq-local gofmt-command "goimports")
  (setq-local whitespace-style (remove 'tab-mark whitespace-style))
  (go-guru-hl-identifier-mode))
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; RUST
(use-package rust-mode
  :config
  (setq rust-mode-on-save t))
(defun my-rust-mode-hook ()
  (setq indent-tabs-mode nil))
(add-hook 'rust-mode-hook 'my-rust-mode-hook)

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-margin-mode 1)
  (diff-hl-flydiff-mode 1))

(use-package elixir-mode)
(use-package exunit)

(use-package terraform-mode)
(use-package dockerfile-mode)
(use-package processing-mode)
(use-package rjsx-mode)
(use-package js2-mode)
(use-package tide)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(use-package origami
  :config
  (global-origami-mode))

(use-package helm
  :quelpa (helm :fetcher github :repo "emacs-helm/helm")
  :config

  (require 'helm-autoloads)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
  (define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  ;; (setq helm-split-window-inside-p t ; open helm buffer inside current window, not occupy whole other window
  ;; helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
  helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
  ;; helm-scroll-amount 8 ; scroll 8 lines other window using M-<next>/M-<prior>
  helm-ff-file-name-history-use-recentf t
  ;; helm-echo-input-in-header-line t


  ;; (add-hook 'helm-minibuffer-set-up-hook
  ;;     'spacemacs//helm-hide-minibuffer-maybe)

  ;; (setq helm-autoresize-max-height 0)
  ;; (setq helm-autoresize-min-height 20)
  ;; (helm-autoresize-mode 1)
  (setq helm-split-window-default-side 'right)

  (setq helm-ff-fuzzy-matching nil)
  (helm-mode 1))

(use-package helm-swoop)
(setq helm-fzf-executable "fzf")
(setq helm-fzf-source
      (helm-build-async-source "fzf"
        :candidates-process 'helm-fzf--do-candidate-process
        :filter-one-by-one 'identity
        :requires-pattern nil
        :action 'helm-find-file-or-marked
        :candidate-number-limit 100))

(defun helm-fzf--do-candidate-process ()
  (let* ((cmd-args (-filter 'identity (list helm-fzf-executable
                                            "-f"
                                            helm-pattern)))
         (proc (apply 'start-file-process "helm-fzf" helm-buffer cmd-args)))
    (prog1 proc
      (set-process-sentinel
       (get-buffer-process helm-buffer)
       #'(lambda (process event)
         (helm-process-deferred-sentinel-hook
          process event (helm-default-directory)))))))

(defun helm-fzf (directory)
  (interactive "D")
  (let ((default-directory directory))
    (helm :sources '(helm-fzf-source)
          :buffer "*helm-fzf*")))

(defun helm-fzf-same (directory)
  (interactive "D")
  (let ((default-directory directory))
    (helm :sources '(helm-fzf-source)
          :buffer "*helm-fzf*"
          :input (file-name-base (buffer-file-name)))))

(setq helm-ripgrep-executable "rg")
(setq helm-ripgrep-source
      (helm-build-async-source "ripgrep"
        :candidates-process 'helm-ripgrep--do-candidate-process
        :filter-one-by-one 'identity
        :requires-pattern nil
        :action 'helm-ripgrep-action
        :candidate-number-limit 100))

(defun helm-ripgrep--do-candidate-process ()
  (let* ((cmd-args (-filter 'identity (list helm-ripgrep-executable
                                            "--no-heading" "--vimgrep" "-n"
                                            "--hidden"
                                            "--max-columns" "400"
                                            "--no-ignore"
                                            "-g" "!.git"
                                            "-g" "node_modules"
                                            "--"
                                            helm-pattern)))
         (proc (apply 'start-file-process "helm-ripgrep" helm-buffer cmd-args)))
    (prog1 proc
      (set-process-sentinel
       (get-buffer-process helm-buffer)
       #'(lambda (process event)
         (helm-process-deferred-sentinel-hook
          process event (helm-default-directory)))))))

(defun helm-ripgrep (directory)
  (interactive "D")
  (let ((default-directory directory))
    (helm :sources '(helm-ripgrep-source)
          :buffer "*helm-ripgrep*")))

(defun helm-ripgrep-action (candidate)
  (let* ((split (split-string candidate ":"))
         (lineno (string-to-number (nth 1 split)))
         (col (string-to-number (nth 2 split)))
         (loc-fname (car split))
         (tramp-method (file-remote-p (or helm-ff-default-directory
                                          default-directory) 'method))
         (tramp-host (file-remote-p (or helm-ff-default-directory
                                        default-directory) 'host))
         (tramp-prefix (concat "/" tramp-method ":" tramp-host ":"))
         (fname (if tramp-host
                    (concat tramp-prefix loc-fname) loc-fname)))
    (find-file fname)
    (helm-goto-line lineno)
    (forward-char (- col 1))))


(use-package helm-descbinds)
(use-package helm-gtags)
;; (use-package nyan-mode
;;   :config
;;   (nyan-mode)
;;   (nyan-toggle-wavy-trail))

;; whitespace stuff
(global-whitespace-mode 1)
(setq-default whitespace-display-mappings
      '(
        ;; (space-mark ?\  [?·] [?.])        ; space - middle dot
        (space-mark ?\xA0 [?¤] [?_])    ; hard space - currency sign
        ;; NEWLINE is displayed using the face `whitespace-newline'
        ;; (newline-mark ?\n [?$ ?\n])       ; eol - dollar sign
        ;; (newline-mark ?\n    [?↵ ?\n] [?$ ?\n]); eol - downwards arrow
        ;; (newline-mark ?\n    [?¶ ?\n] [?$ ?\n]); eol - pilcrow
        ;; (newline-mark ?\n    [?¯ ?\n]  [?$ ?\n]); eol - overscore
        ;; (newline-mark ?\n    [?¬ ?\n]  [?$ ?\n]); eol - negation
        ;; (newline-mark ?\n    [?° ?\n]  [?$ ?\n]); eol - degrees
        ;;
        ;; WARNING: the mapping below has a problem.
        ;; When a TAB occupies exactly one column, it will display the
        ;; character ?\xBB at that column followed by a TAB which goes to
        ;; the next TAB column.
        ;; If this is a problem for you, please, comment the line below.
        (tab-mark ?\t [?» ?\t] [?\\ ?\t]) ; tab - right guillemet
        ))
;; (setq-default whitespace-style
;;               '(face
;;                 tabs spaces trailing lines space-before-tab newline
;;                 indentation empty space-after-tab
;;                 space-mark tab-mark newline-mark))
(setq whitespace-style (remove 'spaces whitespace-style))
(setq whitespace-style (remove 'lines whitespace-style))
(setq whitespace-style (remove 'tabs whitespace-style))
(add-to-list 'whitespace-style 'lines-tail)
(setq whitespace-line-column 120)
(set-face-attribute 'whitespace-trailing nil :background "darkred" :foreground "gray30")
(set-face-attribute 'whitespace-empty nil :background "darkred" :foreground "gray30")

;; perl stuff
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("zrperl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("raku" . raku-mode))
(add-to-list 'auto-mode-alist '("\\.t\\'" . cperl-mode))

(add-hook 'c-mode-hook
          (lambda () (modify-syntax-entry ?_ "w")))

(add-hook 'conf-toml-mode-hook
          (lambda () (modify-syntax-entry ?_ "w")))
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

(setq-default indent-tabs-mode nil)

(setq backup-by-copying t            ; Don't delink hardlinks
      delete-old-versions t          ; Clean up the backups
      version-control t              ; Use version numbers on backups,
      kept-new-versions 5            ; keep some new versions
      kept-old-versions 2)           ; and some old ones, too

;; (setq vc-handled-backends (delq 'Git vc-handled-backends))
(setq visible-bell 1)

(require 'eglot)
(add-to-list 'eglot-server-programs (list 'elixir-mode (expand-file-name "elixir-ls/language_server.sh" user-emacs-directory)))

;; org mode stuff
(setq org-agenda-files (list (expand-file-name "~/todo.org")))
(setq org-highest-priority ?A)
(setq org-lowest-priority ?C)
(setq org-default-priority ?A)
(setq org-agenda-window-setup "current-window")
(setq org-capture-templates
      '(("t" "todo" entry (file+headline "~/todo.org" "Tasks") "* TODO [#A] %?\n  %i\n   %a")))

(tool-bar-mode -1)
(menu-bar-mode -1)
(which-function-mode 1)

(diminish 'global-whitespace-mode)
(diminish 'auto-revert-mode)
(diminish 'helm-mode)
(diminish 'yas-minor-mode)

(server-start)
