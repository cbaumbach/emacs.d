(defun cb/move-beginning-of-line-or-buffer (arg)
  (interactive "P")
  (if arg (beginning-of-buffer) (move-beginning-of-line nil)))

(defun cb/move-end-of-line-or-buffer (arg)
  (interactive "P")
  (if arg (end-of-buffer) (move-end-of-line nil)))
