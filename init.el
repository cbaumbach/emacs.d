;;; PACKAGES

(require 'package)
(setq package-selected-packages
      '())

(require 'cl-lib)
(unless (cl-every 'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (package-install-selected-packages))

(when (< emacs-major-version 27)
  (package-initialize))


;;; CUSTOM FILE

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))


;;; MISCELLANEOUS

(menu-bar-mode 0)

(setq ring-bell-function 'ignore)

(setq make-backup-files nil)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(fset 'yes-or-no-p 'y-or-n-p)

(column-number-mode)

(setq read-buffer-completion-ignore-case t)
