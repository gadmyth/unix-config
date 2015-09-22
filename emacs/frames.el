(defun maximize-frame (frame)

(set-frame-parameter frame 'fullscreen 'maximized))
(add-to-list 'after-make-frame-functions 'maximize-frame)

(global-set-key (kbd "C-x 3") (lambda () (interactive) (select-window (split-window-right))))
(if (boundp 'tool-bar-mode) (tool-bar-mode -1))
(if (boundp 'menu-bar-mode) (menu-bar-mode -1))
(if (boundp 'blink-cursor-mode) (blink-cursor-mode -1))
(if (boundp 'scroll-bar-mode) (scroll-bar-mode -1))
(put 'scroll-left 'disabled nil)

(provide 'frames)
