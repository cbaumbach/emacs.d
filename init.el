;;; PACKAGES

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-selected-packages
      '(avy
        dracula-theme))

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


;;; APPEARANCE

(load-theme 'dracula)

(menu-bar-mode 0)


;;; MISCELLANEOUS

(setq ring-bell-function 'ignore)

(setq make-backup-files nil)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(fset 'yes-or-no-p 'y-or-n-p)

(column-number-mode)

(setq read-buffer-completion-ignore-case t)


;;; AVY

(with-eval-after-load "avy"
  (setq avy-style 'at)
  (setq avy-keys '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m
                   ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
                   ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  (set-face-attribute 'avy-lead-face nil
                      :foreground "green"
                      :background "black")
  (setq avy-background t))

(autoload 'avy-goto-char "avy"
  "Jump to the currently visible CHAR." t)

(global-set-key (kbd "C-c C-SPC") 'avy-goto-char)
