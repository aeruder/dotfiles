(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org)

;; Standard key bindings
(global-set-key "\C-col" 'org-store-link)
(global-set-key "\C-coa" 'org-agenda)
(global-set-key "\C-cob" 'org-iswitchb)

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
(global-set-key (kbd "\C-coc") 'org-capture)
(setq org-default-notes-file "~/org/todo.org")

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

(provide 'init-org)
