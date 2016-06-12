;;; package --- script-extends.el
;;; Commentary:
;;; Code:
(defconst +SCRIPT-EXTENDS-DIR+ (expand-file-name "~/el-extends"))

(defun load-extend-script-files ()
  "This function load all the script under the +SCRIPT-EXTENDS-DIR+."
  (interactive)
  (mapc (lambda (script-file)
          (load-file script-file))
        (directory-files +SCRIPT-EXTENDS-DIR+ t ".*?\\.el$")))

(provide 'script-extends)
;;; script-extends.el ends here
