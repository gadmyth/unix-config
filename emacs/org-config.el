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
        ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
         "* %A %^g %i")
        ("f" "Someday" entry (file+headline "~/org/task.org" "Someday")
         "* %?")
        ))

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

;;; superscripts
(setq-default org-use-sub-superscripts '{})
(setq-default org-export-with-sub-superscripts '{})

;;; babel language
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)
   (plantuml . t)
   (dot . t)
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

(provide 'org-config)
;;; org-config.el ends here
