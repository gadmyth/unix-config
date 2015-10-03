(require 'ac-emoji)

(set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)

(add-hook
 'swift-mode-hook
 (lambda ()
   (mapc 
    (lambda (item)
      (let* ((key (plist-get item :key))
             (key (substring key 1 (1- (length key))))
             (codepoint (plist-get item :codepoint)))
        (define-abbrev swift-mode-abbrev-table key codepoint)))
    ac-emoji--data)))

(add-hook 'swift-mode-hook 'ac-emoji-setup)


(provide 'emoji-config)