(defun yas-cperl-mode-gen-package ()
    (yas-cperl-mode-gen-package-from-file-name
        (file-relative-name
            (buffer-file-name)
            (projectile-project-root))))
(defun yas-cperl-mode-gen-package-from-file-name (filename)
  (replace-regexp-in-string
   "/" "::"
   (replace-regexp-in-string
    "\\(^\\|.*/\\)b?lib/" ""
    (file-name-sans-extension filename))))
