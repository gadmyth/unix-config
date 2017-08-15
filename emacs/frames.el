;;; package --- frames.el
;;; Commentary:
;;; Code:

(defvar *max-frame-width* 0)
(defvar *max-frame-height* 0)

(defun maximize-frame (frame)
  "FRAME: ."
  (set-frame-parameter frame 'fullscreen 'maximized)
  (setq *max-frame-width*  (frame-width frame)
        *max-frame-height* (frame-height frame)))

(add-to-list 'after-make-frame-functions 'maximize-frame)

(defun set-suitable-frame-size ()
  "."
  (interactive)
  (let ((frame (selected-frame)))
    (if (frame-parameter frame 'fullscreen)
      (set-frame-parameter frame 'fullscreen nil))
    (set-frame-size
     frame
     (round (* *max-frame-width* 0.5))
     (round (* *max-frame-height* 0.8)))))

(global-set-key (kbd "C-x 3") (lambda () (interactive) (select-window (split-window-right))))
(if (boundp 'tool-bar-mode) (tool-bar-mode -1))
(if (boundp 'menu-bar-mode) (menu-bar-mode -1))
(if (boundp 'blink-cursor-mode) (blink-cursor-mode -1))
(if (boundp 'scroll-bar-mode) (scroll-bar-mode -1))
(put 'scroll-left 'disabled nil)

(provide 'frames)
;;; frames.el ends here
