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

(provide 'yas-config)
;;; yas-config.el ends here
