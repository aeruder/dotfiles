(defun yas-cperl-mode-gen-package ()
  (yas-cperl-mode-gen-package-from-file-name
   (file-relative-name
    (buffer-file-name)
    (projectile-project-root))))

(defun yas-cperl-mode-gen-package-from-file-name (filename)
  (replace-regexp-in-string
   "/" "::"
   (replace-regexp-in-string
    "\\(?:^\\|.*/\\)\\(?:tests\\|b?lib\\)/" ""
    (file-name-sans-extension filename))))

(setq yas-cperl-mode-package-regexp "^\\(require\\|use\\)[[:space:]]+%s[[:space:]]*\\(?:;\\|[[:space:]]\\|$\\)")

(defun yas-cperl-mode--package-insert-point ()
  (save-excursion
    (save-match-data
      (goto-char (point-max))
      (or
       (and
	(re-search-backward "^\\(require\\|use\\)[[:space:]]+" nil t 1)
	(progn (forward-char) (re-search-forward "^" nil t 1)))
       (point-max)))))

(defun yas-cperl-mode--add-package (pkg &optional use)
  (save-excursion
    (goto-char (yas-cperl-mode--package-insert-point))
    (insert (format "%s %s%s;"
		    "use"
		    pkg
		    (or (and use "") " ()")))
    (newline)))

(defun yas-cperl-mode--has-package (pkg)
  (save-excursion
    (save-match-data
      (condition-case err
	  (progn
	    (goto-char (point-min))
	    (re-search-forward
	     (format
	      yas-cperl-mode-package-regexp
	      (regexp-quote pkg))
	     nil nil 1)

	    (match-data))
	(search-failed nil)))))

(defun yas-cperl-mode-push-package (pkg &optional use)
  (unless (yas-cperl-mode--has-package pkg)
    (run-with-idle-timer 0 nil 'yas-cperl-mode--add-package pkg use))
  "")
