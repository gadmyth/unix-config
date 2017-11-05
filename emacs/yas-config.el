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

(defun new-snippet-with-region ()
  "."
  (interactive)
  (let ((content (buffer-substring-no-properties (region-beginning) (region-end))))
    (yas-new-snippet)
    (with-current-buffer yas-new-snippet-buffer-name
      (goto-char (point-max))
      (insert content))))

(provide 'yas-config)
;;; yas-config.el ends here
