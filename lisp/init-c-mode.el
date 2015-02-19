;;; Make .h files use c++ mode
(add-to-list 'auto-mode-alist
             '("\\.[h]\\'" . c++-mode))

;;; Indentation settings
(add-hook 'c-mode-common-hook
    '(lambda () (c-mode-default)))
(defun c-mode-linux()
    "Linux/u-boot indent settings"
    (interactive)
    (setq c-default-style "linux"
          c-basic-offset 8
          tab-width 8
          whitespace-tab-width 8
          indent-tabs-mode t)
    (whitespace-mode t))
(defun c-mode-gnu()
    "GNU indent settings"
    (interactive)
    (setq c-default-style "gnu"
          c-basic-offset 2
          tab-width 8
          whitespace-tab-width 8
          indent-tabs-mode t)
    (whitespace-mode t))
(defun c-mode-default()
    "My default indentation settings for C modes"
    (interactive)
    (setq c-default-style "linux"
          c-basic-offset 4
          tab-width 4
          whitespace-tab-width 4
          indent-tabs-mode nil)
    (whitespace-mode t))
(setq-default tab-width 8
              indent-tabs-mode nil)
(global-set-key (kbd "C-c TAB l") 'c-mode-linux)
(global-set-key (kbd "C-c TAB g") 'c-mode-gnu)
(global-set-key (kbd "C-c TAB 4") 'c-mode-default)


(provide 'init-c-mode)
