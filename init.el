;;; PACKAGES

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(when (< emacs-major-version 27)
  (package-initialize))

(setq package-selected-packages
      '(avy
        dracula-theme
        ess))

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

(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))


;;; MISCELLANEOUS

(set-language-environment "UTF-8")

(setq ring-bell-function 'ignore)

(setq make-backup-files nil)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(fset 'yes-or-no-p 'y-or-n-p)

(column-number-mode)

(setq read-buffer-completion-ignore-case t)

(setq scroll-conservatively 999)


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


;;; ESS

(setq ess-style 'OWN)

(with-eval-after-load "ess"
  (setq ess-own-style-list
        '((ess-indent-offset . 4)
          (ess-offset-arguments . prev-line)
          (ess-offset-arguments-newline . prev-line)
          (ess-offset-block . prev-line)
          (ess-offset-continued . straight)
          (ess-align-nested-calls "ifelse")
          (ess-align-arguments-in-calls)
          (ess-align-continuations-in-calls)
          (ess-align-blocks)
          (ess-indent-from-lhs)
          (ess-indent-from-chain-start)
          (ess-indent-with-fancy-comments)))
  (ess-add-style 'OWN ess-own-style-list))

;; Use single hash character for all comments.
(add-hook 'ess-mode-hook (lambda () (setq comment-add 0)))
