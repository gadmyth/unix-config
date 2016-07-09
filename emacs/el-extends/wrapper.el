;;; package --- wrapper.el
;;; Commentary:
;;; Code:

(defvar *wrapper-content*)

(defun mk-import-at-point ()
  "."
  (interactive)
  (mk-wrapper "#import \"%s.h\""))

(defun mk-defun-at-point ()
  "."
  (interactive)
  (mk-wrapper "- (void)%s {\n    \n}"))

(defun mk-wrapper (format)
  "FORMAT: ."
  (interactive "sFormat: ")
  (wrapping
   '(lambda (origin-content)
      (format format origin-content))))

(defun wrapping (wrapper)
  "WRAPPER: ."
  (let* ((word (word-at-point))
        (import (funcall wrapper word)))
    (setq *wrapper-content* import)
    (message import)))

(defun insert-wrapper-content ()
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
    (insert "];")))

(defun add-pair-around-region (pair-left pair-right)
  "PAIR-LEFT: , PAIR-RIGHT: ."
  (interactive)
  (if (region-active-p)
      (let ((b (region-beginning))
            (e (region-end)))
        (deactivate-mark)
        (goto-char e)
        (insert pair-right)
        (goto-char b)
        (insert pair-left))))

(defun wrap-paren ()
  "."
  (interactive)
  (add-pair-around-region "(" ")"))

(defun wrap-bracket ()
  "."
  (interactive)
  (add-pair-around-region "[" "]"))

(defun wrap-brace ()
  "."
  (interactive)
  (add-pair-around-region "{" "}"))

(defun wrap-single-quote ()
  "."
  (interactive)
  (add-pair-around-region "'" "'"))

(defun wrap-double-quote ()
  "."
  (interactive)
  (add-pair-around-region "\"" "\""))

(defun wrap-p-pair ()
  "."
  (interactive)
  (add-pair-around-region "<p>" "</p>"))

(defun wrap-red-span ()
  "."
  (interactive)
  (add-pair-around-region "<span style=\"color: red\">" "</span>"))

(defun wrap-lisp-not ()
  "."
  (interactive)
  (add-pair-around-region "(not " ")"))


(provide 'wrapper)
;;; wrapper.el ends here