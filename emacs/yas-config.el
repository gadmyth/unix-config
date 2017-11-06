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

(defun yasnippet-goto-dir ()
  "."
  (interactive)
  (ivy-read "Goto yasnippet dir: " (yas-snippet-dirs)
            :action (lambda (dir)
                      (dired dir))))

(defun yasnippet-major-mode-goto-dir ()
  "."
  (interactive)
  (ivy-read "Choose yasnippet dir: "(yas-snippet-dirs)
            :action (lambda (dir)
                      (dired (format "%s/%s" dir major-mode)))))

(provide 'yas-config)
;;; yas-config.el ends here
