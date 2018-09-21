;;; package --- windows.el
;;; Commentary:
;;; Code:

(global-linum-mode 0)
(global-visual-line-mode t)
(global-auto-revert-mode t)

(require 'linum-relative)
(show-paren-mode 1)
(setq winner-mode t)
(setq column-number-mode t)
(setq line-number-mode t)
(setq size-indication-mode t)

(require 'windmove)
(windmove-default-keybindings)
(window-numbering-mode 1)


(defun quit-help-window ()
  "."
  (interactive)
  (if-let ((help-window (get-buffer-window "*Help*")))
      (quit-window nil help-window)))

(global-set-key (kbd "C-x q") 'quit-help-window)

(provide 'windows)
;;; windows.el ends here
