;;; package --- my init file
;;; Commentary:

;;; Code:
(defvar *no-site-file*)
(setq *no-site-file* nil)

(when (not *no-site-file*)
  (let ((init-file-name (expand-file-name "~/emacs.init.el")))
    (when (file-exists-p init-file-name)
      (load-file init-file-name))))

;;; .emacs ends here