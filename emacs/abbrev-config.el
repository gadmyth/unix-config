(require 'evil)

(define-key evil-insert-state-map (kbd "TAB") 'expand-abbrev)

;; for objc-mode
(define-abbrev objc-mode-abbrev-table "ai" "allocinit")
(define-abbrev objc-mode-abbrev-table "cd" "class-def")
(define-abbrev objc-mode-abbrev-table "fd" "func-def")
(define-abbrev objc-mode-abbrev-table "gfd" "gfunc-def")
(define-abbrev objc-mode-abbrev-table "bi" "block-impl")
(define-abbrev objc-mode-abbrev-table "bi" "block-impl")
(define-abbrev objc-mode-abbrev-table "bp" "block-parameter")
(define-abbrev objc-mode-abbrev-table "fb" "func-bracket")


(provide 'abbrev-config)