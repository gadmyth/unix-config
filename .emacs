;;; package --- my init file
;;; Commentary:

;;; Code:
(defvar *load-config-from-initial-file*)
(setq *load-config-from-initial-file* t)

(when *load-config-from-initial-file*
  (let ((init-file-name (expand-file-name "~/emacs.init.el")))
    (when (file-exists-p init-file-name)
      (load-file init-file-name))))

;;; .emacs ends here