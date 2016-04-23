(evil-mode 1)
(require 'evil-visualstar)

(setq evil-emacs-state-cursor  '("#ae7865" box))
(setq evil-normal-state-cursor '("gray" box))
(setq evil-visual-state-cursor '("yellow" box))
(setq evil-insert-state-cursor '("gray" hollow))
(setq evil-motion-state-cursor '("red" box))
(define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
(define-key evil-insert-state-map (kbd "C-j") 'evil-complete-next)
(define-key evil-insert-state-map (kbd "C-k") 'evil-complete-previous)
(define-key evil-insert-state-map (kbd "C-h") 'evil-backward-char)
(define-key evil-insert-state-map (kbd "C-l") 'evil-forward-char)
(define-key evil-insert-state-map (kbd "C-n") 'evil-next-line)
(define-key evil-insert-state-map (kbd "C-p") 'evil-previous-line)

(global-set-key (kbd "C-6") 'evil-buffer)

(require 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-word-mode)
(define-key evil-normal-state-map (kbd "S-SPC") 'ace-jump-char-mode)
(define-key evil-normal-state-map (kbd "M-SPC") 'ace-jump-line-mode)
(define-key evil-normal-state-map (kbd "S-M-SPC") 'ace-jump-buffer)

(defun is-char? (c)
  (or (and (<= ?A c) (<= c ?Z))
	  (and (<= ?a c) (<= c ?z))))

(delq nil
	  (mapcar (lambda (x)
		  (if (and
			   (eq 'marker (type-of (cdr x))) ;; this can use markerp
			   (is-char? (car x)))
			x
			nil))
		evil-markers-alist))


(add-hook 'slime-repl-mode-hook
		  (lambda ()
            (evil-mode)
			(evil-emacs-state)))

(add-hook 'inferior-emacs-lisp-mode-hook
		  (lambda ()
            (evil-mode)
			(evil-emacs-state)))

(add-hook 'term-mode-hook
		  (lambda ()
            (evil-mode)
			(evil-emacs-state)))

(add-hook 'calendar-mode-hook
		  (lambda ()
            (evil-mode)
			(evil-emacs-state)
			(scale-large 2)))

(add-hook 'org-agenda-mode-hook
		  (lambda ()
			(scale-large 2)))

(add-hook 'erc-mode-hook
		  (lambda ()
            (evil-mode)
			(evil-emacs-state)))

(add-hook 'ielm-mode-hook
		  (lambda ()
            (evil-mode)
			(evil-emacs-state)))

;; when back to evil mode we auto save buffer
(defadvice evil-normal-state (after auto-save last activate)
  (when (evil-normal-state-p)
   (if (and (buffer-file-name) (buffer-modified-p))
        (save-buffer)
      ))) 
      
(provide 'evil-config)