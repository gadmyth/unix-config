(setq ts-init (current-time))
(load-theme 'wombat)
(add-to-list 'load-path (expand-file-name "~/emacs"))
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(defalias 'yes-or-no-p 'y-or-n-p)
(require 'alpha)
(transparency-set-value 92)
(window-numbering-mode 1)
(if (boundp 'tool-bar-mode) (tool-bar-mode -1))
(if (boundp 'menu-bar-mode) (menu-bar-mode -1))
(if (boundp 'scroll-bar-mode) (scroll-bar-mode -1))
(put 'scroll-left 'disabled nil)
(global-linum-mode t)
(require 'linum-relative)
(show-paren-mode 1)
(setq winner-mode t)
(setq column-number-mode t)
(setq line-number-mode t)

(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)
(setq inhibit-startup-message t)
(setq frame-title-format "%f")

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(setq default-abbrev-mode t)
(setq save-abbrevs t)
(global-unset-key (kbd "C-SPC"))
(setq erc-nick "gadmyth")

(setq-default intent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default default-tab-width 4)
(setq-default tab-width 4)
(when (eq window-system 'x)
  (setq x-select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
  (setq x-stretch-cursor t)
  )

(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(setq current-language-environment "UTF-8")
(setq default-input-method "eim-wb")
(setq locale-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setenv "LC_CTYPE" "zh_CN.UTF-8")

(setenv "http_proxy" "127.0.0.1:8087")
(setenv "https_proxy" "127.0.0.1:8087")

(global-set-key (kbd "C-x j") 'ace-jump-word-mode)
(global-set-key (kbd "C-x C-j C-c") 'ace-jump-char-mode)
(global-set-key (kbd "C-x C-j C-l") 'ace-jump-line-mode)
(global-set-key (kbd "C-x C-j C-b") 'ace-jump-buffer)

(setq backup-directory-alist (quote (("." . "~/.backups"))))

(evil-mode 1)
(define-key evil-insert-state-map (kbd "C-a") 'evil-beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'evil-end-of-line)
(require 'wcy-desktop)
(wcy-desktop-init)
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator " :: ") 
(require 'workspace)
(if (not (file-exists-p def-dir))
  (mkdir def-dir)
  (setq default-directory def-dir))

(require 'translate)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-x") 'smex)
(ido-mode t)
(setq ido-enable-flex-matching t)

;(require 'mail-config)
;(require 'el-server)
;(require 'clojure-mode)

(when *load-slime*
  (add-to-list 'load-path (expand-file-name *slime-path*))
  (require 'slime)
  (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
  (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
  (setq slime-net-coding-system 'utf-8-unix)
  (setq slime-lisp-implementations
		'((sbcl (*lisp-bin-path*) :coding-system utf-8-unix)))
  )

(yas/initialize)
(global-set-key (kbd "C-;") 'yas/expand)
;;(yas/load-directory "~/.emacs.d/snippets")
(yas/global-mode 1)

(let ((path (if (eq window-system 'ns)
				(shell-command-to-string "source ~/.bashrc; echo $PATH")
			  (shell-command-to-string "source ~/.profile; echo $PATH"))))
  (setenv "PATH" path)
  (setq exec-path
		(append (split-string-and-unquote path ":") exec-path))) 

(when (eq window-system 'x)
  (tabbar-mode t))

;; scale-amount has been defined in workspace.el
(defvar *mac-scale-amount* 2)
(defvar *linux-scale-amount* 3)
(defun scale-large (&optional files)
  (let ((scale-amount
		 (if (eq window-system 'ns) *mac-scale-amount* *linux-scale-amount*)))
	(text-scale-set scale-amount)))

(add-hook 'find-file-hook
	  (lambda ()
	    (progn
	      (scale-large)
	      (let ((coding-system-for-read 'utf-8))
		(interactive)
		(revert-buffer t t t)))))

;; install lua-mode
(setq auto-mode-alist
	  (let* ((lst auto-mode-alist)
			(no-png-list (remove-if (lambda (x) (equal "\\.png\\'" (car x))) lst))
			(new-png-list 
			 (append no-png-list
					 '(("\\.mm\\'" . objc-mode)
					   ("\\.sur\\.png\\'" . lua-mode)
					   ("\\.trt\\.png\\'" . lua-mode)
					   ("\\.lng\\.png\\'" . lua-mode)
					   ("\\.lua\\.png\\'" . lua-mode)
					   ("\\([^s][^u][^r]\\)\\.png\\'" . image-mode) ;;negative-lookahead not supported
					   ("\\([^t][^r][^t]\\)\\.png\\'" . image-mode) 
					   ("\\([^l][^u][^a]\\)\\.png\\'" . image-mode) 
					   ("\\([^l][^n][^][^g]\\)\\.png\\'" . image-mode)))))
		new-png-list))


(defun is-char? (c)
  (or (and (>= c ?A) (<= c ?Z))
	  (and (>= c ?a) (<= c ?z))))

(delq nil
	  (mapcar (lambda (x)
		  (if (and
			   (eq 'marker (type-of (cdr x))) ;; this can use markerp
			   (is-char? (car x)))
			x
			nil))
		evil-markers-alist))

(setq org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://docs.huihoo.com/homepage/shredderyin/main.css\"/>")

(when (not (eq window-system 'x)) (server-start))
(message "end : %.2f" (float-time (time-since ts-init)))
