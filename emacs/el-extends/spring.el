;;; package --- spring.el
;;; Commentary:
;;; Code:

(require 'counsel)
(require 'source-jump)

(defun java-goto-class (class &optional finish-block)
  "CLASS, FINISH-BLOCK."
  (interactive "sClass: ")
  (let* ((default-directory (expand-file-name (counsel-locate-git-root)))
         (extension "java")
         (full-cmd (format "git ls-files | egrep \"%s(Impl)?\\>.*?.%s\"" class extension))
         (cands (split-string (shell-command-to-string full-cmd) "\n" t)))
    (if (equal 1 (length cands))
        (progn
          (find-file (car cands))
          (if finish-block (funcall finish-block)))
      (ivy-read "Select class: " (reverse cands) :action
              (lambda (candidate)
                (find-file candidate)
                (if finish-block (funcall finish-block)))))))

(defun java-goto-class-at-point ()
  "."
  (interactive)
  (let ((class (word-at-point)))
    (java-goto-class class)))

(defun java-goto-class-def ()
  "."
  (interactive)
  (let* ((class-name (file-name-base (buffer-name)))
         (regexp (format "^public class\\|interface %s.*$" class-name)))
    (sj-action-with-regexp regexp nil "No class here."
                           (apply-partially #'sj-goto-line-or-select "Line content: "))))

(defun java-goto-method (method)
  "METHOD."
  (interactive "sMethod: ")
  (let ((regexp (format "^.*? %s(.*)[^()]*{\s*$" method)))
    (sj-action-with-regexp regexp nil "No methods here."
                           (apply-partially #'sj-goto-line-or-select "The method: "))))

(defun java-goto-method-at-point ()
  "."
  (interactive)
  (let ((method (word-at-point)))
    (java-goto-method method)))

(defun java-jump-to-definition (variable)
  "VARIABLE."
  (let ((regexp (format "\\<\\([A-Z].*\\)\\>\s*%s\s*?[;),=]" variable))
        class)
    (re-search-backward regexp nil t)
    (setq class (match-string-no-properties 1))
    (message "class: %S" class)
    class))

(defun java-jump-to-definition-at-point ()
  "."
  (interactive)
  (let ((variable (word-at-point)))
    (message variable)
    (java-jump-to-definition variable)))

(defun java-jump-to-class-method ()
  "."
  (interactive)
  (let ((method (word-at-point))
        (current-point (point)))
    (save-excursion
      (re-search-backward "\\." nil t)
      (backward-word)
      (let* ((class (word-at-point))
             (first-char (elt class 0)))
        (goto-char current-point)
        (message "method: %s, class: %s" method class)
        (if (not (and (<= ?A first-char) (<= first-char ?Z)))
            (setq class (java-jump-to-definition class)))
        (if class
          (java-goto-class class
                           #'(lambda () (java-goto-method method))))))))

(defun java-goto-last-property ()
  "."
  (interactive)
  (sj-goto-last-with-regexp "^\s*?private\s*.*;\s*?$" "The property name: " "No property here."))

(defun java-create-property (prop-name type)
  "PROP-NAME, TYPE."
  (interactive "sProperty Name: \nsType: ")
  (java-goto-last-property)
  (move-end-of-line 1)
  (insert "\n\n")
  (insert (format "private %s %s;" type prop-name))
  (indent-region (point) (point)))

(defun java-create-setter-getter (prop-name type)
  "PROP-NAME, TYPE."
  (interactive "sProperty Name: \nsType: ")
  (java-goto-class-def)
  (when (re-search-forward "{" nil t 1)
    (evil-jump-item)
    (previous-line)
    (move-end-of-line 1)
    (insert "\n\n")
    (let ((current-point (point)))
      (yas-expand-snippet-with-params "prop-setter" prop-name type)
      (insert "\n\n")
      (yas-expand-snippet-with-params "prop-getter" prop-name type)
      (indent-region current-point (point)))))

(defun java-create-property-suite (prop-name type)
  "PROP-NAME, TYPE."
  (interactive "sProperty Name: \nsType: ")
  (java-create-property prop-name type)
  (java-create-setter-getter prop-name type))

(defun spring-goto-mapper-xml-file (mapper &optional finish-block)
  "MAPPER, FINISH-BLOCK."
  (interactive "sMapper: \nsMethod: ")
  (let* ((default-directory (expand-file-name (counsel-locate-git-root)))
         (extension "xml")
         (full-cmd (format "git ls-files | egrep \"\<%s\\>.%s\"" mapper extension))
         (cands (split-string (shell-command-to-string full-cmd) "\n" t)))
    (if (equal 1 (length cands))
        (progn
          (find-file (car cands))
          (if finish-block (funcall finish-block)))
      (ivy-read "Select mapper file: " (reverse cands) :action
              (lambda (candidate)
                (find-file candidate)
                (if finish-block (funcall finish-block)))))))

(defun spring-mapper-xml-goto-method (method)
  "METHOD."
  (interactive "sMethod: ")
  (let ((regexp (format "^.*? %s(.*)[^()]*{\s*$" method)))
    (sj-action-with-regexp regexp nil "No methods here."
                           (apply-partially #'sj-goto-line-or-select "The method: "))))

(defun spring-jump-to-mapper-xml-method ()
  "."
  (interactive)
  (let ((method (word-at-point))
        (current-point (point)))
    (save-excursion
      (re-search-backward "\\." nil t)
      (backward-word)
      (let* ((class (word-at-point))
             (first-char (elt class 0)))
        (goto-char current-point)
        (message "method: %s, class: %s" method class)
        (if (not (and (<= ?A first-char) (<= first-char ?Z)))
            (setq class (java-jump-to-definition class)))
        (if class
            (spring-goto-mapper-xml-file class
                                         #'(lambda ()
                                             (sj-goto-last-with-regexp method "The method: " "No method here."))))))))

(provide 'spring)
;;; spring.el ends here