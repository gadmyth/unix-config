;;; package --- xcode.el
;;; Commentary:
;;; Code:

(require 'ivy)

(defun read-xcasset-directory (root-directory imageset-handler)
  "ROOT-DIRECTORY is the root directory of the Assets.xcassets dir, IMAGESET-HANDLER is a lambda or function with a imageset dir parameter."
  (let ((dir (expand-file-name root-directory)))
    (ivy-read "Choose imageset: "
              (split-string (shell-command-to-string (format "find %s -name *.imageset" dir))) :action imageset-handler)))


;;; You should config the *xcode-project-directory* in extra config el file.
(defvar *xcode-project-directory* '())

(defun copy-xcasset-directory (handler)
  "HANDLER: ."
  (ivy-read "Choose project directory: " *xcode-project-directory*
            :action (lambda (project-directory)
                      (read-xcasset-directory project-directory
                                              (lambda (imageset)
                                                (funcall handler imageset))))))

(defun dired-copy-file-to-imageset ()
  "."
  (interactive)
  (when (string-equal major-mode "dired-mode")
    (setq-local current-line-file nil)
    (setq-local current-line-file (dired-file-name-at-point))
    (when current-line-file
      (copy-xcasset-directory (lambda (imageset)
                                (if (or (string-suffix-p "@2x.png" current-line-file)
                                        (string-suffix-p "@3x.png" current-line-file))
                                    (when-let ((suffix (cond ((string-suffix-p "@2x.png" current-line-file) "@2x.png")
                                                             ((string-suffix-p "@3x.png" current-line-file) "@3x.png")
                                                             (t nil))))
                                      (let ((to-file (or (car (split-string (shell-command-to-string (format "find %s -name \"*%s\"" imageset suffix))))
                                                         (format "%s/%s%s" imageset (file-name-base imageset) suffix))))
                                        (message "from: %s\nto: %s" current-line-file to-file)
                                        (copy-file current-line-file to-file t)))))))))


(defun goto-buffer (buffername)
  "BUFFERNAME: ."
  (interactive "bbuffer:")
  (if-let ((buffer (get-buffer buffername)))
      (switch-to-buffer buffer)
    (progn
      (counsel-git buffername))))

(defun objc-goto-with-regexp (regexp prompt empty-message &optional initial-input)
  "REGEXP:, PROMPT:, EMPTY-MESSAGE:, &KEY INITIAL-INPUT: ."
  (interactive)
  (with-current-buffer (current-buffer)
    (let (candidates '())
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (push (list (match-string 0) (line-number-at-pos (point))) candidates)))
      (if (> (length candidates) 0)
          (ivy-read prompt candidates
                    :initial-input initial-input
                    :action
                    (lambda (candidate)
                      (goto-line (cadr candidate))))
        (message empty-message)))))

(defun objc-goto-import ()
  "."
  (interactive)
  (objc-goto-with-regexp "^#import .*$" "The import: " "No import here."))

(defun objc-goto-method ()
  "."
  (interactive)
  (objc-goto-with-regexp "^- (.*).*$" "The method: " "No methods here."))

(defun objc-goto-class ()
  "."
  (interactive)
  (objc-goto-with-regexp "^@interface.*$" "The interface: " "No interface here."))

(defun objc-goto-implementation ()
  "."
  (interactive)
  (objc-goto-with-regexp "^@implementation.*$" "The implementation: " "No implementation here."))

(defun objc-goto-property (&optional initial-input)
  "INITIAL-INPUT: ."
  (interactive)
  (objc-goto-with-regexp "^@property.*$" "The properties: " "No properties here." initial-input))

(defun objc-get-current-function-region ()
  "."
  (interactive)
  (let ((current-pos (point))
        start-point
        end-point
        (func-start-regexp "^- (.*).*$")
        (func-end-regexp "^}"))
    (save-excursion
      (when (re-search-backward func-start-regexp nil t)
        (setq start-point (point))
        (when (re-search-forward func-end-regexp nil t)
          (setq end-point (point)))))
    (message "%S %S" start-point end-point)
    (cons start-point end-point)))

(defun objc-mark-current-function ()
  "."
  (interactive)
  (let* ((function-region (objc-get-current-function-region))
         (start (car function-region))
         (end (cdr function-region)))
    (when (and start end)
      (set-mark start)
      (goto-char end))))

(defun objc-show-ui-hierachy ()
  "."
  (interactive))

(defun objc-goto-file (filename project-directory)
  "FILENAME:, PROJECT-DIRECTORY: ."
  (let ((default-directory project-directory))
    ;; COPY START -- counsel.el's counsel-git command
    (counsel-require-program (car (split-string counsel-git-cmd)))
    (ivy-set-prompt 'counsel-git counsel-prompt-function)
    (setq counsel--git-dir (expand-file-name
                            (counsel-locate-git-root)))
    (let* ((default-directory counsel--git-dir)
           (cands (split-string
                   (shell-command-to-string counsel-git-cmd)
                   "\n"
                   t)))
      ;; COPY END -- counsel.el's counsel-git command
      (let* ((cands (ivy--filter filename cands))
             (cands-length (length cands)))
        (message "%S, cands-length" cands cands-length)
        (cond ((= cands-length 0)
               (message "no candidate found!"))
              ((= cands-length 1)
               (counsel-git-action (car cands)))
              (t
               ;; COPY -- counsel.el's counsel-git command
               (ivy-read "Find file" cands
                         :initial-input filename
                         :action #'counsel-git-action
                         :caller 'counsel-git)))))))


(defun example-goto-property (property-name filename project-directory)
  "PROPERTY-NAME:, FILENAME:, PROJECT-DIRECTORY."
  (objc-goto-file filename project-directory)
  (sleep-for 2)
  (objc-goto-property property-name))

(if-let ((test t))
    (objc-goto-file "FloatProductViewController.m" "~/wealthapp-ios"))

(example-goto-property "common" "FloatProductViewController.m" "~/wealthapp-ios")
(provide 'xcode)
;;; xcode.el ends here