;;; ==== PACKAGES ====================================================

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

(load (concat user-emacs-directory "defuns.el"))


;;; ==== CUSTOM FILE =================================================

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))


;;; ==== APPEARANCE ==================================================

(load-theme 'dracula)

(menu-bar-mode 0)

(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(defun cb/use-terminal-like-font-attributes ()
  "Disable bold font, use one font family and a single height."
  (let ((family (face-attribute 'default :family))
        (height (face-attribute 'default :height)))
    (mapc (lambda (face)
            (set-face-attribute face nil
                                :weight 'normal
                                :family family
                                :height height))
          (face-list))))

(when (display-graphic-p)
  (let ((font-family-list (font-family-list)))
    (catch 'done
      (dolist (font '("UnifontMono" "Consolas"))
        (when (member font font-family-list)
          (set-face-attribute 'default nil
                              :family font
                              :height 120)
          (add-to-list 'initial-frame-alist `(font . ,font))
          (add-to-list 'default-frame-alist `(font . ,font))
          (throw 'done t)))))
  (cb/use-terminal-like-font-attributes)
  (add-hook 'minibuffer-setup-hook
            'cb/use-terminal-like-font-attributes)
  (add-hook 'change-major-mode-after-body-hook
            'cb/use-terminal-like-font-attributes))


;;; ==== MISCELLANEOUS ===============================================

(set-language-environment "UTF-8")

(setq ring-bell-function 'ignore)

(setq make-backup-files nil)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(delete-selection-mode)

(fset 'yes-or-no-p 'y-or-n-p)

(column-number-mode)

(setq read-buffer-completion-ignore-case t)

(setq scroll-conservatively 999)

(setq set-mark-command-repeat-pop t)

;; Prefer side-by-side over stacked for first split provided that
;; split windows would be more than `fill-column` characters wide.
(setq-default fill-column 70)
(when (> (/ (window-total-width) 2) fill-column)
  (setq split-height-threshold (+ (window-total-height) 3) ; values < 3 failed
        split-width-threshold (- (window-total-width) 3))) ;  in gui emacs

(setq-default default-input-method 'german-postfix)

;; Enable all commands that are disabled by default.
(setq disabled-command-function nil)


;;; ==== AVY =========================================================

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


;;; ==== Dired =======================================================

(add-hook 'dired-mode-hook 'dired-hide-details-mode)


;;; ==== Ediff =======================================================

(with-eval-after-load "ediff"
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))


;;; ==== ESS =========================================================

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


;;; ==== Notes =======================================================

(defvar cb/notes-filename "~/notes.org"
  "Path to file where notes should be saved.")

(defun cb/toggle-notes ()
  (interactive)
  (if (string-equal (buffer-file-name) (expand-file-name cb/notes-filename))
      (quit-window)
    (find-file cb/notes-filename)))


;;; ==== Org =========================================================

(setq-default org-adapt-indentation nil)
(setq org-edit-src-content-indentation 0)
(setq org-startup-folded t)
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c ,") 'org-insert-structure-template)))


;;; ==== Python ======================================================

(defun cb/python-shell-send-statement-and-step ()
  (interactive)
  (python-shell-send-statement)
  (python-nav-forward-statement))

(defun cb/python-shell-send-block-and-step ()
  (interactive)
  (mark-paragraph)
  (cb/python-shell-send-region (region-beginning) (region-end) nil t)
  (python-nav-forward-statement))

(defun cb/python-shell-send-region (start end &optional send-main msg)
  (interactive
   (list (region-beginning) (region-end) current-prefix-arg t))
  (python-shell-send-region start end send-main msg)
  (deactivate-mark)
  (goto-char end))

(defun cb/python-shell-send-file (filename)
  (interactive
   (list (read-file-name "File to send: " nil nil t
                         (file-name-nondirectory (buffer-file-name)))))
  (python-shell-send-file filename))

(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "M-p") 'python-nav-backward-statement)
            (local-set-key (kbd "M-n") 'python-nav-forward-statement)
            (local-set-key (kbd "M-a") 'python-nav-backward-defun)
            (local-set-key (kbd "M-e") 'python-nav-forward-defun)
            (local-set-key (kbd "C-c C-n") 'cb/python-shell-send-statement-and-step)
            (local-set-key (kbd "C-c C-c") 'cb/python-shell-send-block-and-step)
            (local-set-key (kbd "C-c C-r") 'cb/python-shell-send-region)
            (local-set-key (kbd "C-c C-l") 'cb/python-shell-send-file)
            (local-set-key (kbd "C-z") 'python-shell-switch-to-shell)
            ;; Don't use python-specific definition of sexps.
            (setq forward-sexp-function nil)))

(add-hook 'inferior-python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-z") 'other-window)
            (local-set-key (kbd "C-c C-z") 'other-window)))


;;; ==== Keybindings =================================================

;; Toggles
(global-unset-key (kbd "C-h t"))
(global-set-key (kbd "C-h t a") 'auto-fill-mode)
(global-set-key (kbd "C-h t n") 'cb/toggle-notes)
(global-set-key (kbd "C-h t t") 'toggle-truncate-lines)

;; Inserts
(global-unset-key (kbd "C-h i"))
(global-set-key (kbd "C-h i p") 'cb/insert-path)

;; Movement
(global-set-key (kbd "C-a") 'cb/move-beginning-of-line-or-buffer)
(global-set-key (kbd "C-e") 'cb/move-end-of-line-or-buffer)

;; Editing
(global-set-key (kbd "M-z") 'zap-up-to-char)
