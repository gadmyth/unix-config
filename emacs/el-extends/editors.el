;;; package --- editors.el
;;; Commentary:
;;; Code:

(defvar *wrapper-content*)

(defun wrapping (wrapper)
  "WRAPPER: ."
  (let* ((word (word-at-point))
        (import (funcall wrapper word)))
    (setq *wrapper-content* import)
    (message import)))

(defun output-wrapper-content ()
  "."
  (interactive)
  (when *wrapper-content*
    (insert-string *wrapper-content*)
    (setq *wrapper-content* nil)))

(defun wrap-line-with-oc-method ()
  "."
  (interactive)
  (save-excursion
    (re-search-backward "^" nil t)
    (re-search-forward " *" nil t 1)
    (insert "[")
    (re-search-forward "$" nil t 1)
    (insert "];")
    (end-of-line)))

(defun wrap-right-part-with-oc-method ()
  "."
  (interactive)
  (save-excursion
    (end-of-line)
    (re-search-backward "= " nil t)
    (re-search-forward " " nil t 1)
    (insert "[")
    (re-search-forward "$" nil t 1)
    (insert "];"))
  (end-of-line))

(defun edit-left-type ()
  "."
  (interactive)
  (end-of-line)
  (re-search-backward "^" nil t)
  (re-search-forward " *" nil t 1)
  (kill-word 1)
  (evil-insert-state))

(defun edit-left-arg ()
  "."
  (interactive)
  (end-of-line)
  (re-search-backward "^" nil t)
  (re-search-forward " " nil t 1)
  (kill-word 1)
  (evil-insert-state))

(defun edit-left-part ()
  "."
  (interactive)
  (end-of-line)
  (re-search-backward "^" nil t)
  (let ((begin (point)))
    (re-search-forward " =" nil t 1)
    (backward-char 2)
    (let ((end (point)))
      (kill-region begin end)))
  (evil-insert-state))

(defun edit-right-part ()
  "."
  (interactive)
  (beginning-of-line)
  (re-search-forward "= " nil t)
  (let ((begin (point)))
    (re-search-forward "$" nil t 1)
    (let ((end (point)))
      (kill-region begin end)))
  (evil-insert-state))

(provide 'editors)
;;; editors.el ends here