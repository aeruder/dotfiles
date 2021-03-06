;;; init-use-package.el --- Get started with use-package in emacs

;; Copyright (C) 2015 Gregory J Stein

;; Author: Gregory J Stein <gregory.j.stein@gmail.com>
;; Keywords: use-package
;; License: none, use this however you want without citation
;; 

;; Code inspired by:
;;      http://stackoverflow.com/a/10093312/3672986
;;      http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html
;;      https://github.com/jwiegley/use-package


;;; Commentary:

;; As Sebastian Wiesner from http://www.lunaryorn.com/ points out, there is a "chicken
;; and egg" problem with use-package, which is capable of automatically downloading and
;; installing packages, but otherwise needs to be downloaded and installed manually.
;; I include the following code in my emacs initialization file so that this process
;; is automatic.


;;; Code:

;; Update package-archive lists
(require 'package)
(setq package-enable-at-startup nil)
(setq quelpa-update-melpa-p nil)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Install 'use-package' if necessary
(unless (package-installed-p 'quelpa)
  (package-refresh-contents)
  (package-install 'quelpa))

;; Enable use-package
(require 'quelpa)

(quelpa
 '(quelpa-use-package
   :fetcher github
   :repo "quelpa/quelpa-use-package"))
(require 'quelpa-use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(quelpa-use-package-activate-advice)

(provide '00-use-package)
;;; init-use-package.el ends here
