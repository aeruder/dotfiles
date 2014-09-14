;;;
;;; Evil mode
;;;

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
(define-key evil-normal-state-map (kbd "]a") 'next-buffer)
(define-key evil-normal-state-map (kbd "[a") 'previous-buffer)
(define-key evil-normal-state-map (kbd ",bo")
  (lambda()
    (interactive)
    (projectile-find-file-in-directory "~/org/")))
(define-key evil-normal-state-map (kbd ",bs")
  (lambda()
    (interactive)
    (projectile-find-file-in-directory "~/.emacs.d/snippets/")))

(define-key evil-normal-state-map (kbd ",ei")
  (lambda()
    (interactive)
    (find-file "~/.emacs.d/init.el")))

(define-key evil-normal-state-map (kbd ", SPC")
  (lambda()
    (interactive)
    (evil-ex-nohighlight)))

(evil-mode t)

(provide 'init-evil)
