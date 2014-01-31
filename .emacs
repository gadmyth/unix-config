'(setq default-frame-alist
'((x 0) (y 0) (height . 180) (width . 270) (menu-bar-lines . 20) (tool-bar-lines . 0)))
(global-unset-key (kbd "C-SPC"))

(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)
(setq inhibit-startup-message t)

(setq default-abbrev-mode t)
(setq save-abbrevs t)

(tool-bar-mode -1)
(menu-bar-mode -1)

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

(put 'scroll-left 'disabled nil)
(show-paren-mode 1)

(setq current-language-environment "UTF-8")
(setq default-input-method "eim-wb")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


(require 'package)
(add-to-list 'package-archives
			 '("melpa" . "http://melpa.milkbox.net/packages/"))
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
(require 'workspace)
;; def-dir has been defined in workspace.el
(if (not (file-exists-p def-dir))
  (mkdir def-dir)
  (setq default-directory def-dir))

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
 '(custom-enabled-themes (quote (wombat)))
 '(haskell-mode-hook (quote (turn-on-haskell-indent turn-on-haskell-indentation)))
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(when *load-slime*
  (add-to-list 'load-path (expand-file-name *slime-path*))
  (require 'slime)
  (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
  (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
  (setq inferior-lisp-program *lisp-bin-path*)
  )
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

(defun tabbar-buffer-groups ()
  "tabbar group"
  (list
   (cond
	((memq major-mode '(java-mode cc-mode))
	 "java")
	((memq major-mode '(nxml-mode xml-mode))
	 ("xml"))
	((memq major-mode '(lua-mode))
	 "lua")
	((memq major-mode '(haskell-mode))
	 "haskell")
	((memq major-mode '(text-mode))
	 "text")
	((string-equal "*" (substring (buffer-name) 0 1))
	 "emacs")
	(t
	 "default"))))

(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

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

 (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)


(global-linum-mode t)
(require 'linum-relative)

(setq linum-use-scale nil)

(defun linum-update-window (win)
  "Update line numbers for the portion visible in window WIN."
  (goto-char (window-start win))
  (let ((line (line-number-at-pos))
        (limit (window-end win t))
        (fmt (cond ((stringp linum-format) linum-format)
                   ((eq linum-format 'dynamic)
                    (let ((w (length (number-to-string
                                      (count-lines (point-min) (point-max))))))
                      (concat "%" (number-to-string w) "d")))))
        (width 0))
    (run-hooks 'linum-before-numbering-hook)
    ;; Create an overlay (or reuse an existing one) for each
    ;; line visible in this window, if necessary.
    (while (and (not (eobp)) (<= (point) limit))
      (let* ((str (if fmt
                      (propertize (format fmt line) 'face 'linum)
                    (funcall linum-format line)))
             (visited (catch 'visited
                        (dolist (o (overlays-in (point) (point)))
                          (when (equal-including-properties
				 (overlay-get o 'linum-str) str)
                            (unless (memq o linum-overlays)
                              (push o linum-overlays))
                            (setq linum-available (delq o linum-available))
                            (throw 'visited t)))))
	     (width-scale (if (and (boundp 'text-scale-mode-step) linum-use-scale)
			    (expt text-scale-mode-step text-scale-mode-amount) 1)))
	(setq width (ceiling (* width-scale (max width (length str)))))
        (unless visited
          (let ((ov (if (null linum-available)
                        (make-overlay (point) (point))
                      (move-overlay (pop linum-available) (point) (point)))))
            (push ov linum-overlays)
            (overlay-put ov 'before-string
                         (propertize " " 'display `((margin left-margin) ,str)))
            (overlay-put ov 'linum-str str))))
      ;; Text may contain those nasty intangible properties, but that
      ;; shouldn't prevent us from counting those lines.
      (let ((inhibit-point-motion-hooks t))
        (forward-line))
      (setq line (1+ line)))
    (set-window-margins win width (cdr (window-margins win)))))
