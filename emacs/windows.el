(global-linum-mode t)
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

(provide 'windows)
