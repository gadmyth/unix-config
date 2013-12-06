'(setq default-frame-alist
'((x 0) (y 0) (height . 180) (width . 270) (menu-bar-lines . 20) (tool-bar-lines . 0)))

(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)
(setq inhibit-startup-message t)

(setq default-abbrev-mode t)
(setq save-abbrevs t)

(setq shell-file-name "/bin/sh")
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)

(setq column-number-mode t)
(setq line-number-mode t)

(setq-default intent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default default-tab-width 4)
(setq-default tab-width 4)
(when (eq window-system 'x)
  (setq x-select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
  (setq x-stretch-cursor t)
  )

(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)
(setq def-dir (expand-file-name "~/workspace/TouchPal"))
(setq default-directory def-dir)
(unless (file-exists-p def-dir)
  (mkdir def-dir))

(put 'scroll-left 'disabled nil)
(setq c-basic-offset 4)
(setq tab-width 4)
(show-paren-mode 1)

(setq current-language-environment "UTF-8")

(setq current-language-environment "UTF-8")
(setq default-input-method "eim-wb")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)


(setq backup-directory-alist (quote (("." . "~/.backups"))))

(setq winner-mode t)

(require 'evil)
(evil-mode 1)
(define-key evil-insert-state-map (kbd "C-a") 'evil-beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'evil-end-of-line)
;(setq viper-mode t)
;(setq viper-ex-style-editing nil)
;(require 'viper)
(add-to-list 'load-path (expand-file-name "~/emacs"))
(require 'wcy-desktop)
(wcy-desktop-init)
;(require 'vimpulse)
;(setq woman-use-own-frame nil)
;(setq woman-use-topic-at-point t)
;;(require 'redo)
;;(require 'rect-mark)


;(require 'color-theme)
;(color-theme-deep-blue)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-enabled-themes (quote (adwaita)))
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

'(add-to-list 'load-path (expand-file-name "~/emacs/slime"))
'(require 'slime)
'(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
'(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
'(setq inferior-lisp-program "/usr/local/bin/ecl")
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)


(require 'clojure-mode)

(require 'yasnippet)
(require 'yasnippet-bundle)
(yas/initialize)
(global-set-key (kbd "C-;") 'yas/expand)
;;(yas/load-directory "~/.emacs.d/snippets")
(yas/global-mode 1)

(require 'alpha)
(transparency-set-value 100)

(require 'magit) 

;(desktop-save-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
	  uniquify-separator " :: ") 
(filesets-init)

(let ((path (shell-command-to-string "source ~/.bashrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
		(append (split-string-and-unquote path ":") exec-path))) 

(require 'tabbar)

(when (eq window-system 'x)
  (tabbar-mode t))

(defun scale-large (&optional files)
  (let ((scale-amount
		 (if (eq window-system 'ns) 2 3)))
	(text-scale-set scale-amount)))

(add-hook 'find-file-hook
		  (lambda ()
			(scale-large)))

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



(global-linum-mode t)
(require 'linum-relative)

