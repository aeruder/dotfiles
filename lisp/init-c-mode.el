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
          indent-tabs-mode t))
(defun c-mode-gnu()
    "GNU indent settings"
    (interactive)
    (setq c-default-style "gnu"
          c-basic-offset 2
          tab-width 8
          indent-tabs-mode t))
(defun c-mode-default()
    "My default indentation settings for C modes"
    (interactive)
    (setq c-default-style "linux"
          c-basic-offset 4
          tab-width 4
          indent-tabs-mode nil))
(setq-default tab-width 8
              indent-tabs-mode nil)

(provide 'init-c-mode)
