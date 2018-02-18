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
    (sj-save-excursion
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

(defconst +java-constant-regexp-format+ "\\(private\\|public\\).*%s.*$\\|^\s*%s(.*).*$")
(defun java-goto-constant (constant)
  "CONSTANT."
  (interactive "sConstant: ")
  (let ((regexp (format +java-constant-regexp-format+ constant constant)))
    (sj-goto-last-with-regexp regexp "The constant's name: " "No constant here.")))

(defun java-goto-constant-at-point ()
  "."
  (interactive)
  (let* ((word (word-at-point)))
    (java-goto-constant word)))

(defun java-show-constant (constant)
  "CONSTANT."
  (interactive "sConstant: ")
  (let ((regexp (format +java-constant-regexp-format+ constant constant))
        (current-point (point)))
    (sj-show-last-with-regexp regexp "The constant's name: " "No constant here."
                              #'(lambda ()
                                  (goto-char current-point)))))

(defun java-show-constant-at-point ()
  "."
  (interactive)
  (let* ((word (word-at-point)))
    (java-show-constant word)))

(defun java-jump-to-class-constant ()
  "."
  (interactive)
  (let ((constant (word-at-point))
        (current-point (point)))
    (sj-save-excursion
     (re-search-backward "\\." nil t)
     (backward-word)
     (let* ((class (word-at-point))
            (first-char (elt class 0)))
       (goto-char current-point)
       (message "constant: %s, class: %s" constant class)
       (if (not (and (<= ?A first-char) (<= first-char ?Z)))
           (setq class (java-jump-to-definition class)))
       (if class
           (java-goto-class class
                            #'(lambda () (java-goto-constant constant))))))))

(defun java-show-class-constant ()
  "."
  (interactive)
  (let ((constant (word-at-point))
        (current-point (point)))
    (sj-save-excursion
     (re-search-backward "\\." nil t)
     (backward-word)
     (let* ((class (word-at-point))
            (first-char (elt class 0)))
       (goto-char current-point)
       (message "constant: %s, class: %s" constant class)
       (if (not (and (<= ?A first-char) (<= first-char ?Z)))
           (setq class (java-jump-to-definition class)))
       (if class
           (java-goto-class class
                            #'(lambda ()
                                (java-show-constant constant)
                                (command-execute 'evil-buffer))))))))

(defconst +java-property-regexp+ "^\s*?private\s*.*;\s*?$")

(defun java-goto-property ()
  "."
  (interactive)
  (sj-goto-with-regexp +java-property-regexp+ "The property name: " "No property here."))

(defun java-goto-last-property ()
  "."
  (interactive)
  (sj-goto-last-with-regexp +java-property-regexp+ "The property name: " "No property here."))

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
         (full-cmd (format "git ls-files | egrep \"\\<%s\\>.%s\"" mapper extension))
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
    (sj-save-excursion
     (re-search-backward "\\." nil t)
     (backward-word)
     (let* ((class (word-at-point))
            (first-char (elt class 0)))
       (goto-char current-point)
       (if (not (and (<= ?A first-char) (<= first-char ?Z)))
           (setq class (java-jump-to-definition class)))
       (message "method: %s, class: %s" method class)
       (if class
           (spring-goto-mapper-xml-file class
                                        #'(lambda ()
                                            (sj-goto-last-with-regexp method "The method: " "No method here."))))))))

(defun spring-git-grep (word)
  "WORD."
  (interactive "sWord: ")
  (let* ((default-directory (expand-file-name (counsel-locate-git-root)))
         (full-cmd (format "git grep %s" word))
         (cands (split-string (shell-command-to-string full-cmd) "\n" t)))
    (ivy-read "Select candidate: " (reverse cands) :action #'(lambda (candidate)
                                                               (require 's)
                                                               (let* ((trim-candidate (s-trim candidate))
                                                                      (array (split-string trim-candidate ":"))
                                                                      (file (first array))
                                                                      (line-content (second array)))
                                                                 (find-file file)
                                                                 (goto-char (point-min))
                                                                 (search-forward line-content))))))

(defun java-property-file-candidates ()
  "."
  (interactive)
  (goto-char (point-min))
  (let ((regexp "^[^#][^=]+=[^=]+$")
        (collections '())
        (end-of-buffer nil)
        (line-begin-pos 0)
        (line-end-pos 0)
        (line-string nil))
    (while (not end-of-buffer)
      (skip-chars-backward "^\n")
      (setq line-begin-pos (point))
      (skip-chars-forward "^\n")
      (setq line-end-pos (point))
      (setq end-of-buffer (equal line-end-pos (point-max)))
      (setq line-string (buffer-substring line-begin-pos line-end-pos))
      (setq line-string-no-properties (buffer-substring-no-properties line-begin-pos line-end-pos))
      (if (string-match regexp line-string-no-properties)
          (push (list line-string (line-number-at-pos (point))) collections))
      (if (not end-of-buffer)
          (next-line)))
    collections))

(defun java-property-file-show-candidates ()
  "."
  (interactive)
  (let ((regexp "^[^#][^=]+=[^=]+$"))
    (sj-action-with-regexp regexp nil "No line content here."
                           (apply-partially #'sj-goto-line-or-select "Line content: "))))

(defun spring-list-application-properties ()
  "."
  (interactive)
  (let* ((default-directory (expand-file-name (counsel-locate-git-root)))
         (extension "java")
         (full-cmd (format "git ls-files | egrep \"%s\"" "application.properties"))
         (cands (split-string (shell-command-to-string full-cmd) "\n" t)))
    (if (equal 1 (length cands))
        (progn
          (find-file (car cands))
          (let ((collections (java-property-file-candidates)))
            (command-execute 'evil-buffer)
            (ivy-read "Select property: " (reverse collections) :action nil)))
      (ivy-read "Select class: " (reverse cands) :action
              (lambda (candidate)
                (find-file candidate)
                (let ((collections (java-property-file-candidates)))
                  (command-execute 'evil-buffer)
                  (ivy-read "Select property: " (reverse collections) :action nil)))))))


(defun spring-git-grep-at-point ()
  "."
  (interactive)
  (let ((word (word-at-point)))
    (spring-git-grep word)))

(provide 'spring)
;;; spring.el ends here