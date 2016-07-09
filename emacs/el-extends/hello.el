;;; package --- hello.el
;;; Commentary:
;;; Code:

(defun hello ()
  (interactive)
  (show-additional-buffer ".emacs"))

(defun show-additional-buffer (buffer)
  (let ((origin-buffer (current-buffer)))
    (switch-to-buffer-other-window buffer)
    (switch-to-buffer-other-window origin-buffer)))

(defun world ()
  (interactive)
  (show-additional-buffer2 ".emacs"))

(defun show-additional-buffer2 (buffer)
  (let ((origin-buffer (current-buffer)))
    (switch-to-buffer-other-frame buffer)
    (switch-to-buffer-other-frame origin-buffer)))

(defun edit-value (regex pattern-group)
  (re-search-backward "^" nil t)
  (re-search-forward regex nil t 1)
  (goto-char (match-beginning pattern-group))
  (delete-region (match-beginning pattern-group) (match-end pattern-group))
  (evil-insert-state))

(defun edit-assign-value ()
  (interactive)
  (edit-value "^ *?\\([^=]*?\\) = \\(.*\\)$" 2))

(defun edit-line ()
  (interactive)
  (edit-value "^[ \t]*?\\([^ \t].*\\)$" 1))

(defun edit-css-prop ()
  (interactive)
  (edit-value "^ *?\\([^:]*?\\) *: *\\(.*?\\)[,;] *?$" 2))

(defun replace-point-word (new-word)
  "NEW-WORD: ."
  (interactive (list (read-string "Replace by: " (car kill-ring))))
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (replace-regexp ".*" new-word nil (car bounds) (cdr bounds)))))

(defun replace-point-word-current-line (new-word)
  "NEW-WORD: ."
  (interactive (list (read-string "Replace by: " (car kill-ring))))
  (save-excursion
    (let ((word (word-at-point)))
      (re-search-backward "^" nil t)
      (replace-regexp word new-word nil (line-beginning-position) (line-end-position)))))

(provide 'hello)
;;; hello.el ends here