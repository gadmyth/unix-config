;;; org-config.el --- config org-mode
(require 'org)

;;; todo-keywords
(setq org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "CLOSING(c)" "PENDING(p)" "PLAN(l)" "|" "DONE(d!)" "ABORT(a@/!)")))

(setq org-todo-keyword-faces '(("PENDING" . (:background "LightGreen" :foreground "black" :weight bold))
                               ("PLAN" . (:background "LightGray" :foreground "white" :weight bold))
                               ("DOING" . (:background "Green" :foreground "white" :weight bold))
                               ("CLOSING" . (:background "Orange" :foreground "white" :weight bold))
                               ("TODO" . (:background "DarkOrange" :foreground "black" :weight bold))
                               ("DONE" . (:background "azure" :foreground "Darkgreen" :weight bold))
                               ("ABORT" . (:background "gray" :foreground "black"))))
(setq org-log-done 'time)

;;; priority
(setq org-highest-priority ?A)
(setq org-lowest-priority ?E)
(setq org-default-priority ?E)
(setq org-priority-faces '((?A . (:background "red" :foreground "white" :weight bold))
                           (?B . (:background "DarkOrange" :foreground "white" :weight bold))
                           (?C . (:background "yellow" :foreground "DarkGreen" :weight bold))
                           (?D . (:background "DodgerBlue" :foreground "black" :weight bold))
                           (?E . (:background "SkyBlue" :foreground "black" :weight bold))
                           (?F . (:background "LightSkyBlue" :foreground "black" :weight bold))))

;;; taglist
;;(setq org-tag-alist '(("@work" . ?w) ("@home" . ?h) ("laptop" . ?l)))

;;; dependencies
(setq org-enforce-todo-dependencies t)

;;; agenda
(setq org-agenda-files '("~/org"))
(setq org-agenda-ndays 14)
(setq org-agenda-include-diary t)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "<f6>") 'org-todo-list)

;;; capture
(setq org-default-notes-file (concat org-directory "/notes.org"))
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+datetree "~/org/task.org")
         "* TODO %^{Decription} %^T %^g\n %i")
        ("T" "Timeline" entry (file+datetree "~/org/timeline.org")
         "* %^{Decription} %^T %^g\n %i")
        ("e" "event" entry (file+datetree "~/org/timeline.org")
         "* %^{Decription} %T :event:\n %i")
        ("r" "routine" entry (file+datetree "~/org/timeline.org")
         "* %^{Decription} %T :routine:\n %i")
        ("p" "problem" entry (file+datetree "~/org/timeline.org")
         "* %^{Decription} %T :problem:\n %i")
        ("P" "plan" entry (file+datetree "~/org/timeline.org")
         "* %^{Decription} %T :plan:\n %i")
        ("s" "summary" entry (file+datetree "~/org/timeline.org")
         "* %^{Decription} %T :summary:\n %i")
        ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
         "* %A %^g %i")
        ("f" "Someday" entry (file+headline "~/org/task.org" "Someday")
         "* %?")
        ))

(require 'org-id)
(setq org-id-link-to-org-use-id t)

;;; org html config
(setq org-html-head
      (format "<style type='text/css'>%s</style>"
              (with-temp-buffer
                (setq default-directory (expand-file-name "~/emacs"))
                (insert-file-contents "org.css")
                (buffer-string))))

(setq org-html-head
      (with-temp-buffer
                (setq default-directory (expand-file-name "~/emacs"))
                (insert-file-contents "theme-readtheorg.style")
                (buffer-string)))

(setq org-html-head
      (format "<style type='text/css'>%s</style><style type='text/css'>%s</style><script type='text/javascript'>%s</script><script type='text/javascript'>%s</script><script type='text/javascript'>%s</script><script type='text/javascript'>%s</script>"
              (with-temp-buffer
                (setq default-directory (expand-file-name "~/emacs/org/res"))
                (insert-file-contents "htmlize.css")
                (buffer-string))
              (with-temp-buffer
                (setq default-directory (expand-file-name "~/emacs/org/res"))
                (insert-file-contents "readtheorg.css")
                (buffer-string))
              (with-temp-buffer
                (setq default-directory (expand-file-name "~/emacs/org/res"))
                (insert-file-contents "jquery.min.js")
                (buffer-string))
              (with-temp-buffer
                (setq default-directory (expand-file-name "~/emacs/org/res"))
                (insert-file-contents "bootstrap.min.js")
                (buffer-string))
              (with-temp-buffer
                (setq default-directory (expand-file-name "~/emacs/org/res"))
                (insert-file-contents "jquery.stickytableheaders.min.js")
                (buffer-string))
              (with-temp-buffer
                (setq default-directory (expand-file-name "~/emacs/org/res"))
                (insert-file-contents "readtheorg.js")
                (buffer-string)))
      )

(defun m/org-html-checkbox (checkbox)
  "Format CHECKBOX into HTML."
  (case checkbox (on "<span class=\"check\">&#x2611;</span>") ; checkbox (checked)
        (off "<span class=\"checkbox\">&#x2610;</span>")
        (trans "<span class=\"checkbox\">&#x229F;")
        (t "")))

(defadvice org-html-checkbox (around sacha activate)
  (setq ad-return-value (m/org-html-checkbox (ad-get-arg 0))))

(setq org-export-preserve-breaks "<br>")

(eval-after-load "org"
  '(progn
	 (setq org-startup-indented t)))

;;; setup org src code color
(setq org-src-fontify-natively t)

;;; src block indent
(setq org-edit-src-content-indentation 0)

;;; inline image size
(setq org-image-actual-width nil)

;;; superscripts
(setq-default org-use-sub-superscripts '{})
(setq-default org-export-with-sub-superscripts '{})

;;; babel language
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)
   (plantuml . t)
   (dot . t)
   (restclient . t)
   (sh . t)
   ))

(setq org-ditaa-jar-path (expand-file-name "~/libs/ditaa.jar"))
(setq org-plantuml-jar-path (expand-file-name "~/libs/plantuml.jar"))

(add-hook 'org-babel-after-execute-hook
          '(lambda () (condition-case nil (org-display-inline-images) (error nil)))
          'append)

(setq org-confirm-babel-evaluate
      '(lambda (lang body)
         "LANG: , BODY."
         (and (not (string= lang "ditaa"))
              (not (string= lang "dot"))
              (not (string= lang "plantuml")))))


(require 'ox-publish)
(setq org-publish-project-alist
      '(("blog-notes"
         :base-directory "~/Documents/homo2/notes" ;存放笔记目录
         :base-extension "org"
         :publishing-directory "~/Documents/homo2/public_html/" ; 导出目录
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :auto-preamble t
         :section-numbers nil
         :author "gadmyth"
         :email "gadmyth@gmail.com"
         :auto-sitemap t
         :sitemap-filename "sitemap.org"
         :sitemap-title "Sitemap"
         :sitemap-sort-files anti-chronologically
         :sitemap-file-entry-format "%d %t")
        ("blog-static"
         :base-directory "~/Documents/homo2/notes"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-extension "~/Documents/homo2/public_html/"
         :recursive t
         :publishing-function org-publish-attachment)
        ("blog" :components ("blog-notes" "blog-static"))
        ))

(provide 'org-config)
;;; org-config.el ends here
