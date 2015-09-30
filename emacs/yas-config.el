(require 'yasnippet)
(add-hook
 'yas-global-mode-hook
 (lambda ()
   (add-to-list 'yas-snippet-dirs (expand-file-name "~/emacs/snippets"))))

(provide 'yas-config)