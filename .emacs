(setq ts-init (current-time))
(defun maximize-frame (frame)
  (let* (
		 (width (- (/ (x-display-pixel-width) (frame-char-width frame)) 4))
		 (height (- (/ (x-display-pixel-height) (frame-char-height frame)) 4))
		 (alist (list (cons 'left 0) (cons 'top 0) (cons 'width width) (cons 'height height))))
	(modify-frame-parameters frame alist)))
(add-to-list 'after-make-frame-functions 'maximize-frame)
(maximize-frame (selected-frame))
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
(if (boundp 'blink-cursor-mode) (blink-cursor-mode -1))
(if (boundp 'scroll-bar-mode) (scroll-bar-mode -1))
(put 'scroll-left 'disabled nil)
(global-linum-mode t)
(global-visual-line-mode t)
(global-auto-revert-mode t)
(require 'linum-relative)
(show-paren-mode 1)
(setq winner-mode t)
(setq column-number-mode t)
(setq line-number-mode t)
(setq size-indication-mode t)
(require 'diff-hl)
(global-diff-hl-mode t)
(require 'windmove)
(windmove-default-keybindings)

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
(require 'erc)
(setq erc-nick "gadmyth")
(setq erc-hide-list '("JOIN" "QUIT" "PART" "NICK" "MODE"))
(defun start-erc (passwd)
  (interactive (list (read-passwd "passwd: ")))
  (erc :server erc-default-server :port erc-default-port :nick erc-nick :password passwd))
(setq erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#wecase")))

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
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(eval-after-load "vc-hooks" '(define-key vc-prefix-map "=" 'vc-ediff))
(eval-after-load "helm" '(setq helm-split-window-default-side 'below))
(eval-after-load "textmate" '(add-to-list '*textmate-project-roots* ".svn"))
(eval-after-load "xcscope" '(add-to-list 'cscope-indexer-suffixes "*.java"))
(eval-after-load "org"
  '(progn
	 (setq org-startup-indented t)
	 (setq org-default-notes-file (concat org-directory "/notes.org"))))

(setq backup-directory-alist (quote (("." . "~/.backups"))))

(evil-mode 1)
(require 'evil-visualstar)
(smartparens-global-mode)
(yas-global-mode)
(global-auto-complete-mode)
(setq evil-emacs-state-cursor  '("#ae7865" box))
(setq evil-normal-state-cursor '("gray" box))
(setq evil-visual-state-cursor '("yellow" box))
(setq evil-insert-state-cursor '("green" bar))
(setq evil-motion-state-cursor '("red" box))
(define-key evil-insert-state-map (kbd "C-a") 'evil-beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'evil-end-of-line)
(define-key evil-motion-state-map (kbd "C-6") 'evil-buffer)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-word-mode)
(define-key evil-normal-state-map (kbd "S-SPC") 'ace-jump-char-mode)
(define-key evil-normal-state-map (kbd "M-SPC") 'ace-jump-line-mode)
(define-key evil-normal-state-map (kbd "S-M-SPC") 'ace-jump-buffer)
(require 'wcy-desktop)
(wcy-desktop-init)
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator " :: ") 
(setq uniquify-after-kill-buffer-p t)
(require 'autoloads)
(require 'workspace)
(if (not (file-exists-p def-dir))
  (mkdir def-dir))
(setq default-directory def-dir)
(switch-proxy nil)
(require 'pretty-mode+)

(require 'annot)
(global-set-key (kbd "M-x") 'smex)
(ido-mode t)
(setf helm-buffers-fuzzy-matching t)
(setq ido-enable-flex-matching t)

(when *load-slime*
  (require 'slime)
  (slime-setup '(slime-repl slime-scratch slime-fuzzy slime-c-p-c slime-banner slime-autodoc slime-fancy slime-fancy-inspector))
  (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
  (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
  (setq slime-net-coding-system 'utf-8-unix
		slime-lisp-implementations
		`((sbcl (,*lisp-bin-path*) :coding-system utf-8-unix))
		slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
  (global-set-key (kbd "C-c s") 'slime-selector)
  )


(let ((path (if (eq window-system 'ns)
				(shell-command-to-string "source ~/.bashrc; echo $PATH")
			  (shell-command-to-string "source ~/.profile; echo $PATH"))))
  (setenv "PATH" path)
  (setq exec-path
		(append (split-string-and-unquote path ":") exec-path))) 

(require 'elscreen)
(elscreen-set-prefix-key "\C-k")
(elscreen-start)

;; scale-amount has been defined in workspace.el
(defvar *mac-scale-amount* 2)
(defvar *linux-scale-amount* 3)
(defun scale-large (&optional amount)
  (let ((scale-amount
		 (if amount amount
		   (if (eq window-system 'ns) *mac-scale-amount* *linux-scale-amount*))))
	(text-scale-set scale-amount)))

(add-hook 'find-file-hook
	  (lambda ()
	    (progn
	      (scale-large)
	      (let ((coding-system-for-read 'utf-8))
			(interactive)
			(revert-buffer t t t)
			(require 'textmate)
			(textmate-mode)
			(require 'xcscope)
			(cscope-minor-mode)
			(pretty-mode)))))

(add-hook 'slime-repl-mode-hook
		  (lambda ()
			(evil-emacs-state)))

(add-hook 'inferior-emacs-lisp-mode-hook
		  (lambda ()
			(evil-emacs-state)))

(add-hook 'term-mode-hook
		  (lambda ()
			(evil-emacs-state)))

(add-hook 'eshell-mode-hook
		  (lambda ()
			(evil-emacs-state)))

(add-hook 'calendar-mode-hook
		  (lambda ()
			(evil-emacs-state)
			(scale-large 2)))

(add-hook 'org-agenda-mode-hook
		  (lambda ()
			(scale-large 2)))

(add-hook 'erc-mode-hook
		  (lambda ()
			(evil-emacs-state)))

(add-hook 'ielm-mode-hook
		  (lambda ()
			(evil-emacs-state)))

(add-hook 'after-save-hook
		  (lambda () (if (string= (buffer-name) ".emacs")
					(byte-compile-file (expand-file-name "~/.emacs")))))

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

(let ((+org-css+ "org.css"))
  (setq org-html-head (format "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\"/>" +org-css+)))
(defun m/org-html-checkbox (checkbox)
  "Format CHECKBOX into HTML."
  (case checkbox (on "<span class=\"check\">&#x2611;</span>") ; checkbox (checked)
        (off "<span class=\"checkbox\">&#x2610;</span>")
        (trans "<code>[-]</code>")
        (t "")))
(defadvice org-html-checkbox (around sacha activate)
  (setq ad-return-value (m/org-html-checkbox (ad-get-arg 0))))

(if (and (not (eq window-system 'x))
	   (boundp 'server-process)
	   (null server-process))
	(server-start))

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map
                [remap eshell-pcomplete]
                'helm-esh-pcomplete)))

(setq calendar-chinese-all-holidays-flag t)
(setq christian-holidays nil)
(setq hebrew-holidays nil)
(setq hebrew-holidays-1 nil)
(setq hebrew-holidays-2 nil)
(setq hebrew-holidays-3 nil)
(setq hebrew-holidays-4 nil)
(setq islamic-holidays nil)
(setq bahai-holidays nil)
	          
(message "end : %.2f" (float-time (time-since ts-init)))
