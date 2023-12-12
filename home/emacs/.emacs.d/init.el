;; keybinds
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; stuff dealing with setting up package, use-package, quelpa, etc.

(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(setq use-package-always-ensure t)
(use-package quelpa)
(quelpa
 '(quelpa-use-package
   :fetcher github
   :repo "quelpa/quelpa-use-package"))
(require 'quelpa-use-package)
(quelpa-use-package-activate-advice)

(setq package-native-compile t)
(setq warning-suppress-types '((comp)))
(use-package diminish)

;; easier keybinds
(use-package general
  :diminish general-override-mode
  :config
  (general-override-mode))

(use-package emacs
  :ensure nil
  :general
  (:states '(normal visual motion)
           :keymaps 'override
           "SPC b" '(nil :which-key "buffer")
           "SPC b d" 'kill-current-buffer
           "SPC b D" 'aeruder/kill-all-buffers
           "SPC b m" 'aeruder/kill-other-buffers
           "SPC b x" 'aeruder/reset-buffer-major-mode

           "SPC f" '(nil :which-key "file")
           "SPC f f" 'find-file
           "SPC f R" 'read-only-mode
           "SPC f w" 'save-buffer
           "SPC f b" '(nil :which-key "bookmark")
           "SPC f b b" 'bookmark-jump
           "SPC f b s" 'bookmark-save
           "SPC f b l" 'bookmark-bmenu-list

           "SPC f e" '(nil :which-key "edit")
           "SPC f e d" 'aeruder/edit-init-file
           "SPC f e D" 'aeruder/ls-dotfiles
           "SPC f e t" 'aeruder/edit-todo-file
           "SPC f n" '(nil :which-key "new")
           "SPC f n s" 'aeruder/new-scratch-file
           "SPC f s" 'server-edit
           "SPC f x" 'aeruder/make-file-executable
           "SPC f y" '(nil :which-key "yank")
           "SPC f y y" 'aeruder/copy-file-path-rel
           "SPC f y Y" 'aeruder/copy-file-path-abs
           "SPC f y g" 'aeruder/copy-url-path
           "SPC f y G" 'aeruder/copy-url-path-line

           "SPC g" '(nil :which-key "git")
           "SPC g l" '(nil :which-key "log")

           "SPC h" '(nil :which-key "help")
           "SPC h d" '(nil :which-key "describe")
           "SPC h d b" 'describe-bindings
           "SPC h d c" 'describe-char
           "SPC h d f" 'describe-function
           "SPC h d k" 'describe-key
           "SPC h d p" 'describe-package
           "SPC h d t" 'describe-theme
           "SPC h d v" 'describe-variable
           "SPC h n" 'view-emacs-news

           "SPC p" '(nil :which-key "project")
           "SPC p p" 'projectile-switch-project
           "SPC p f" 'aeruder/fzf-projectile
           "SPC p F" 'aeruder/fzf-same-projectile
           "SPC p s" 'projectile-ripgrep
           "SPC p S" 'aeruder/ripgrep-projectile
           "SPC p t" 'helm-gtags-find-tag

           "SPC q" '(nil :which-key "quit")
           "SPC q q" 'spacemacs/frame-killer
           "SPC q Q" 'aeruder/quit

           "SPC w" '(nil :which-key "window")
           "SPC w =" 'balance-windows
           "SPC w d" 'delete-window
           "SPC w D" 'delete-other-window
           "SPC w s" 'split-window-below
           "SPC w v" 'split-window-right

           "SPC x" '(nil :which-key "text")
           "SPC x a" '(nil :which-key "align")
           "SPC x a =" 'aeruder/align=
           "SPC x a S" 'aeruder/reformat-sql
           )

  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Do not allow the cursor in the minibuffer prompt
  :config
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  (setq-default display-line-numbers 'relative)
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; relative line numbers
  (setq-default display-line-numbers 'relative)
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

  (setq visible-bell 1)

  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (which-function-mode 1)

  (xterm-mouse-mode 1)
  (setq xterm-set-window-title t)

  (setq last-paste-to-osx nil)

  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

                                        ; vim keybindings
(use-package evil
  :diminish evil-mode
  :general
  (:states '(normal visual motion)
           :keymaps 'override
           "SPC w h" 'evil-window-left
           "SPC w l" 'evil-window-right
           "SPC w j" 'evil-window-down
           "SPC w k" 'evil-window-up
           "SPC x +" 'evil-numbers/inc-at-pt
           "SPC x -" 'evil-numbers/dec-at-pt)

  :init
  (setq evil-want-C-i-jump nil)         ; necessary for terminal because C-i is tab
  (setq evil-want-integration nil)
  (setq evil-want-keybinding nil)
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(use-package evil-escape
  :diminish evil-escape-mode
  :init
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.2)
  :config
  (add-to-list 'evil-escape-excluded-major-modes 'magit-status-mode)
  (add-to-list 'evil-escape-excluded-major-modes 'magit-revision-mode)
  (add-to-list 'evil-escape-excluded-major-modes 'magit-diff-mode)
  (evil-escape-mode))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))
(use-package evil-commentary)

(use-package evil-collection
  :config
  (delq 'diff-mode evil-collection-mode-list)
  (delq 'outline evil-collection-mode-list)
  (evil-collection-init))

                                        ; this is the thing that opens up help for keybindings
(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-enable-extended-define-key t)
  :config
  (which-key-mode))




;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))


;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; Example configuration for Consult
(use-package consult
  :general
  (:states '(normal visual motion)
           :keymaps 'override
           "SPC b b" 'consult-buffer
           "SPC b B" 'consult-bookmark
           "SPC b r" 'consult-recent-file
           )

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package
  birds-of-paradise-plus-theme
  :config (load-theme 'birds-of-paradise-plus t))

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode))

(use-package magit

  :general

  (:states '(normal visual motion)
           :keymaps 'override
           "SPC g R" 'magit-refresh-all
           "SPC g f" 'magit-file-dispatch
           "SPC g b" 'magit-blame
           "SPC g g" 'magit
           "SPC g h" 'magit-reflog
           "SPC g l A" 'magit-log-author
           "SPC g l R" 'magit-reflog
           "SPC g l a" 'magit-log-all
           "SPC g l f" 'magit-log-buffer-file
           "SPC g l r" 'magit-reflog-current
           "SPC g l x" 'magit-rebase-interactive
           "SPC g r" 'magit-refresh
           "SPC g s" 'magit-status)
  )

(use-package vi-tilde-fringe
  :diminish vi-tilde-fringe-mode
  :config
  (global-vi-tilde-fringe-mode))

(use-package whitespace
  :ensure nil
  :init
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
  (setq whitespace-line-column 120)

  :config
  (global-whitespace-mode 1)
  (add-to-list 'whitespace-style 'lines-tail)
  (set-face-attribute 'whitespace-trailing nil :background "darkred" :foreground "gray30")
  (set-face-attribute 'whitespace-empty nil :background "darkred" :foreground "gray30"))

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

(server-start)
