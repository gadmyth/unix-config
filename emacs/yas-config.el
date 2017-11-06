;;; package --- yas-config.el
;;; Commentary:
;;; Code:

(require 'yasnippet)
(add-hook
 'yas-global-mode-hook
 (lambda ()
   (add-to-list 'yas-snippet-dirs (expand-file-name "~/snippets"))
   (add-to-list 'yas-snippet-dirs (expand-file-name "~/emacs/snippets"))))

(setq yas-indent-line 'fixed)

(defun goto-yasnippet-dirs ()
  "."
  (interactive)
  (ivy-read "Goto yasnippet dir: " (yas-snippet-dirs)
            :action (lambda (dir)
                      (dired dir))))

(provide 'yas-config)
;;; yas-config.el ends here
