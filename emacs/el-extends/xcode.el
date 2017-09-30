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


(provide 'xcode)
;;; xcode.el ends here