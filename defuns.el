(defun cb/move-beginning-of-line-or-buffer (arg)
  (interactive "P")
  (if arg (beginning-of-buffer) (move-beginning-of-line nil)))

(defun cb/move-end-of-line-or-buffer (arg)
  (interactive "P")
  (if arg (end-of-buffer) (move-end-of-line nil)))

(defun cb/insert-path (arg)
  "Prompt for path and insert it at point.  If prefix arg is
non-nil, use \\ instead of / as path separator."
  (interactive "P")
  (let ((path (read-file-name "Path: "))
        (sep (if arg "\\\\\\\\" "/")))
    (insert (replace-regexp-in-string "/" sep path))))
