(setq ts-init (current-time))
(defun maximize-frame (frame)
(set-frame-parameter frame 'fullscreen 'maximized))
(add-to-list 'after-make-frame-functions 'maximize-frame)
(load-theme 'tango)
(add-to-list 'load-path (expand-file-name "~/emacs"))
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
;; the slime should git clone from github
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/slime"))
(defvar required-packages
  (list 'alpha 'diff-hl 'windmove 'textmate
		'helm 'xcscope 'org-mode 'evil
		'evil-visualstar 'pretty-mode 'slime 'slime-fuzzy
		'elscreen 'projectile 'annot 'yasnippet
		'ov 'ace-jump-buffer 'ace-jump-mode 'elnode 'flycheck
		'dirtree 'cal-china-x 'hydra))
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

(defalias 'yes-or-no-p 'y-or-n-p)
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
(setq erc-autojoin-channels-alist '(("freenode.net" "#wecase" "#emacs")))

(setq-default indent-tabs-mode nil)
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
(setq mode-require-final-newline nil)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(eval-after-load "vc-hooks" '(define-key vc-prefix-map "=" 'vc-ediff))
(eval-after-load "vc-hooks" '(define-key vc-prefix-map "k" 
							   (lambda (&optional historic not-urgent)
								 (interactive (list current-prefix-arg t))
								 (mapc (lambda (file) (delete-file file)) (cadr (vc-deduce-fileset t))))))
(eval-after-load "helm" '(setq helm-split-window-default-side 'right))
(global-set-key (kbd "C-,") 'helm-imenu-anywhere)
(eval-after-load "textmate" '(add-to-list '*textmate-project-roots* ".svn"))
(eval-after-load "xcscope" '(add-to-list 'cscope-indexer-suffixes "*.java"))
(eval-after-load "org"
  '(progn
	 (setq org-startup-indented t)))

(setq backup-directory-alist (quote (("." . "~/.backups"))))

(evil-mode 1)
(require 'evil-visualstar)
(smartparens-global-mode)
;(mapc (lambda (key) (delete key sp-trigger-keys)) '("\"" "'" "`"))
(yas-global-mode)
(global-auto-complete-mode)
(setq evil-emacs-state-cursor  '("#ae7865" box))
(setq evil-normal-state-cursor '("gray" box))
(setq evil-visual-state-cursor '("yellow" box))
(setq evil-insert-state-cursor '("green" bar))
(setq evil-motion-state-cursor '("red" box))
(define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
(global-set-key (kbd "C-6") 'evil-buffer)
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
(require 'alpha)
(transparency-set-value *default-trans-value*)
(require 'pretty-mode+)
(require 'org-config)
(require 'hydra-config)

(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(ido-mode t)
(setf helm-buffers-fuzzy-matching t)
(setq ido-enable-flex-matching t)

(when *load-slime*
  (require 'slime-autoloads)
  (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
  (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
  (setq slime-net-coding-system 'utf-8-unix)
  (setq slime-lisp-implementations `((sbcl (,*lisp-bin-path*) :coding-system utf-8-unix)))
  (setq slime-contribs '(slime-fancy))
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

(require 'projectile)
(projectile-global-mode)
(setq projectile-indexing-method 'native)
(setq projectile-enable-caching t)
(setq projectile-file-exists-remote-cache-expire nil)

;; scale-amount has been defined in workspace.el
(defvar *mac-scale-amount* 2)
(defvar *linux-scale-amount* 3)
(defun scale-large (&optional amount)
  (let ((scale-amount
		 (if amount amount
		   (if (eq window-system 'ns) *mac-scale-amount* *linux-scale-amount*))))
	(dotimes (i scale-amount)
	  (ov-double-height))))

(add-hook 'find-file-hook
	  (lambda ()
	    (progn
	      ;(scale-large)
	      (let ((coding-system-for-read 'utf-8))
			(interactive)
			(revert-buffer t t t)
			(scale-large)
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
	   (or (not (boundp 'server-process))
		   (null server-process)))
	(ignore-errors
	 (server-start)))

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map
                [remap eshell-pcomplete]
                'helm-esh-pcomplete)))

(setq mark-holidays-in-calendar t)
(require 'cal-china-x)
(setq calendar-chinese-all-holidays-flag t)
(setq christian-holidays nil)
(setq hebrew-holidays nil)
(setq hebrew-holidays-1 nil)
(setq hebrew-holidays-2 nil)
(setq hebrew-holidays-3 nil)
(setq hebrew-holidays-4 nil)
(setq islamic-holidays nil)
(setq bahai-holidays nil)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/one"))
(require 'one)

(require 'helm-dash)
(setq helm-dash-common-docsets '("Android"))

(require 'magit)
(setq magit-repo-dirs '("~/emacs-workspace" "~/unix-config"))

(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'after-init-hook (lambda () (require 'dirtree)))

(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)

(eval-after-load 'dired
  '(progn
     (define-key dired-mode-map (kbd "c") 'dired-create-file2)
     (defun dired-create-file2 (file)
       "Create a file called FILE. If FILE already exists, signal an error."
       (interactive
        (list (read-file-name "Create file: " (dired-current-directory))))
       (let* ((expanded (expand-file-name file))
              (try expanded)
              (dir (directory-file-name (file-name-directory expanded)))
              new)
         (if (file-exists-p expanded)
             (error "Cannot create file %s: file exists" expanded))
         ;; Find the topmost nonexistent parent dir (variable `new')
         (while (and try (not (file-exists-p try)) (not (equal new try)))
           (setq new try
                 try (directory-file-name (file-name-directory try))))
         (when (not (file-exists-p dir))
           (make-directory dir t))
         (write-region "" nil expanded t)
         (when new
           (dired-add-file new)
           (dired-move-to-filename))))))

(load-must-files)

;(require 'annot)
	          
(maximize-frame (selected-frame))
(message "end : %.2f" (float-time (time-since ts-init)))
