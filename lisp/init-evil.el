;;;
;;; Evil mode
;;;

(require-package 'evil)
(require 'evil-rebellion)
(require-package 'evil-matchit)
(require-package 'evil-numbers)

(setq evil-search-module 'evil-search
      evil-want-C-u-scroll t
      evil-want-C-w-in-emacs-state t)
(setq evil-want-C-i-jump nil)
(require 'evil)
(define-key evil-normal-state-map (kbd ";") 'evil-ex)

(global-evil-matchit-mode 1)

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

;;Make evil-mode up/down operate in screen lines instead of logical lines
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;;,b is our prefix for searching through open files, opening new files,
;;etc.
(global-set-key (kbd "C-c b a") 'org-agenda)
(global-set-key (kbd "C-c b b") 'ido-switch-buffer)
(global-set-key (kbd "C-c b c") 'ido-find-file)
(global-set-key (kbd "C-c b g") 'ag)
(global-set-key (kbd "C-c b f") 'projectile-find-file)
(global-set-key (kbd "C-c b o")
  (lambda()
    (interactive)
    (projectile-find-file-in-directory "~/org/")))
(global-set-key (kbd "C-c b s")
  (lambda()
    (interactive)
    (projectile-find-file-in-directory "~/.emacs.d/snippets/")))
(global-set-key (kbd "C-c b i")
  (lambda()
    (interactive)
    (projectile-find-file-in-directory "~/.emacs.d/")))
(global-set-key (kbd "C-c b R R R")
  (lambda()
    (interactive)
    (revert-buffer)))

(define-key evil-normal-state-map (kbd "]a") 'next-buffer)
(define-key evil-normal-state-map (kbd "[a") 'previous-buffer)

(define-key evil-normal-state-map (kbd ", SPC")
  (lambda()
    (interactive)
    (evil-ex-nohighlight)))

(evil-mode t)

(provide 'init-evil)
