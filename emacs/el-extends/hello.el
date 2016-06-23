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


(defun add-pair-around-region (pair-left pair-right)
  (interactive)
  (if (region-active-p)
      (let ((b (region-beginning))
            (e (region-end)))
        (deactivate-mark)
        (goto-char e)
        (insert pair-right)
        (goto-char b)
        (insert pair-left))))

(defun add-paren-around-region ()
  (interactive)
  (add-pair-around-region "(" ")"))

(defun add-bracket-around-region ()
  (interactive)
  (add-pair-around-region "[" "]"))

(defun add-brace-around-region ()
  (interactive)
  (add-pair-around-region "{" "}"))

(defun add-single-quote-around-region ()
  (interactive)
  (add-pair-around-region "'" "'"))

(defun add-double-quote-around-region ()
  (interactive)
  (add-pair-around-region "\"" "\""))

(defun wrap-p-pair-around-region ()
  (interactive)
  (add-pair-around-region "<p>" "</p>"))

(defun wrap-red-span ()
  (interactive)
  (add-pair-around-region "<span style=\"color: red\">" "</span>"))

(defun wrap-lisp-not ()
  "."
  (interactive)
  (add-pair-around-region "(not " ")"))

(provide 'hello)
;;; hello.el ends here