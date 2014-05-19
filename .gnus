(setq user-full-name "gadmyth")
(setq user-mail-address "gadmyth@gmail.com")

(auto-image-file-mode)
(setq mm-inline-large-images t)
(add-to-list 'mm-attachment-override-types "image/*")

(setq gnus-select-method 
	  '(nnimap "gmail"
		(nnimap-address "imap.gmail.com")
		(nnimap-server-port 993)
		(nnimap-stream ssl)))

(setq message-send-mail-function 'smtpmail-send-it
	  smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
	  smtpmail-auth-credentials '(("smtp.gmail.com" 587 "gadmyth@gmail.com" nil))
	  smtpmail-default-smtp-server "smtp.gmail.com"
	  smtpmail-smtp-server "smtp.gmail.com"
	  smtpmail-smtp-service 587
	  gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
