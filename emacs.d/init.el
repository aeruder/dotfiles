(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/org/z4.org" "~/org/todo.org" "~/org/redilink.org" "~/org/gitlab.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#2e3436" :foreground "#eeeeec" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "Anka/Coder"))))
 '(whitespace-empty ((t (:foreground "black" :background "#800"))))
 '(whitespace-line ((t (:foreground "black" :background "#aa0"))))
 '(whitespace-space-before-tab ((t (:foreground "black" :background "#800"))))
 '(whitespace-tab ((t (:foreground "#aa0" :background "#333"))))
 '(whitespace-trailing ((t (:foreground "#800" :background "#aa0")))))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;;; from purcell/emacs.d
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

;; make backup to a designated dir, mirroring the full path

(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* (
        (backupRootDir "~/.emacs.d/emacs-backup/")
        (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path, ⁖ “C:”
        (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") ))
        )
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath
  )
)

(setq make-backup-file-name-function 'my-backup-file-name)

(package-initialize)

;;;
;;; Org mode
;;;

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org)

;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq org-directory "~/org")
;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file "~/org/refile.org")

;; ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
;; (setq org-capture-templates
;;       (quote (("t" "todo" entry (file "~/git/org/refile.org")
;;                "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
;;               ("r" "respond" entry (file "~/git/org/refile.org")
;;                "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
;;               ("n" "note" entry (file "~/git/org/refile.org")
;;                "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
;;               ("j" "Journal" entry (file+datetree "~/git/org/diary.org")
;;                "* %?\n%U\n" :clock-in t :clock-resume t)
;;               ("w" "org-protocol" entry (file "~/git/org/refile.org")
;;                "* TODO Review %c\n%U\n" :immediate-finish t)
;;               ("m" "Meeting" entry (file "~/git/org/refile.org")
;;                "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
;;               ("p" "Phone call" entry (file "~/git/org/refile.org")
;;                "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
;;               ("h" "Habit" entry (file "~/git/org/refile.org")
;;                "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

; org agenda -- leave in emacs mode but add j & k

(add-hook 'org-agenda-mode-hook
   (lambda ()
     (define-key org-agenda-mode-map "j" 'evil-next-line)
     (define-key org-agenda-mode-map "k" 'evil-previous-line)))

;;;
;;; Evil mode
;;;

(require-package 'evil)

(setq evil-search-module 'evil-search
      evil-want-C-u-scroll t
      evil-want-C-w-in-emacs-state t)
(setq evil-want-C-i-jump nil)
(require 'evil)
(define-key evil-normal-state-map (kbd ";") 'evil-ex)

(require-package 'evil-matchit)
(global-evil-matchit-mode 1)

(require-package 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

; Set up the jk macro
(define-key evil-insert-state-map "j" #'cofi/maybe-exit)
(evil-define-command cofi/maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "j")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?j)
               nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?k))
    (delete-char -1)
    (set-buffer-modified-p modified)
    (push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
                          (list evt))))))))
;; Remap org-mode meta keys for convenience
(mapcar (lambda (state)
            (evil-declare-key state org-mode-map
                (kbd "M-l") 'org-metaright
                (kbd "M-h") 'org-metaleft
                (kbd "M-k") 'org-metaup
                (kbd "M-j") 'org-metadown
                (kbd "M-L") 'org-shiftmetaright
                (kbd "M-H") 'org-shiftmetaleft
                (kbd "M-K") 'org-shiftmetaup
                (kbd "M-J") 'org-shiftmetadown))
        '(normal insert))

;;Make evil-mode up/down operate in screen lines instead of logical lines
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;;,b is our prefix for searching through open files, opening new files,
;;etc.
(define-key evil-normal-state-map (kbd ",ba") 'org-agenda)
(define-key evil-normal-state-map (kbd ",bb") 'ido-switch-buffer)
(define-key evil-normal-state-map (kbd ",bc") 'ido-find-file)
(define-key evil-normal-state-map (kbd ",bg") 'ag)
(define-key evil-normal-state-map (kbd ",bf") 'projectile-find-file)
(define-key evil-normal-state-map (kbd ",bo")
  (lambda()
    (interactive)
    (projectile-find-file-in-directory "~/org/")))

(define-key evil-normal-state-map (kbd ",ei")
  (lambda()
    (interactive)
    (find-file "~/.emacs.d/init.el")))

(define-key evil-normal-state-map (kbd ", SPC")
  (lambda()
    (interactive)
    (evil-ex-nohighlight)))

(evil-mode t)
;;;
;;; Ace jump
;;;
(require-package 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-char-mode)
(setq ace-jump-mode-scope 'window)

;;;
;;; Yasnippet
;;;
(require-package 'yasnippet)
(yas-global-mode 1)
;;Allow quick manual triggering of snippets
(define-key evil-normal-state-map (kbd "C-c s") 'yas-insert-snippet)
(define-key evil-insert-state-map (kbd "C-c s") 'yas-insert-snippet)

(require-package 'whitespace)
(global-linum-mode t)
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;;;
;;; ag
;;;
(require-package 'ag)

;;;
;;; Git modes
;;;
(require-package 'git-commit-mode)
(require-package 'gitconfig-mode)
;;(require-package 'git-rebase-mode)
(require-package 'gitignore-mode)
(require-package 'gitattributes-mode)

;;;
;;; Buffer switching
;;;
(require-package 'flx)
(require-package 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(require-package 'ido-vertical-mode)
(ido-vertical-mode 1)

;;;
;;; Miscellaneous
;;;
(setq gc-cons-threshold 20000000)

;;;
;;; Rainbow Mode
;;;
(require-package 'rainbow-mode)

;;;
;;; WhiteSpace
;;;
(require-package 'whitespace)

(setq whitespace-line-column 80)
(setq whitespace-style
  '(empty face lines-tail space-before-tab tabs tab-mark trailing))
(global-whitespace-mode t)

;;;
;;; Projectile
;;;

(require-package 'projectile)
(projectile-global-mode)
(setq projectile-globally-ignored-files (append '("*~") projectile-globally-ignored-files))


;;;
;;; Magit
;;;

(require-package 'magit)
