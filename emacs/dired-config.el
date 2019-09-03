;;; package --- dired-config.el
;;; Commentary:
;;; Code:

(require 'dired)
(setq dired-listing-switches "-lFaGh1v --group-directories-first")

(require 'dired++)

(require 'dired-x)
(setq-default dired-omit-files-p t)
(add-to-list 'dired-omit-extensions ".DS_Store")

(require-if-installed
 'dired-narrow
 (define-key dired-mode-map (kbd "/") 'dired-narrow))

(provide 'dired-config)
;;; dired-config.el ends here