(require 'pretty-mode)


(defun load-extra-pretty ()
  (when (eq major-mode 'java-mode)
	(pretty-regexp "import" "I")
	(pretty-regexp "false" "×")
	(pretty-regexp "Boolean.FALSE" "×")
	(pretty-regexp "true" "✓")
	(pretty-regexp "Boolean.TRUE" "✓")
	(pretty-regexp "TextUtils.isEmpty" "⌘")
	(pretty-regexp "!" "¬")
	(pretty-regexp "&&" "∧")
	(pretty-regexp "||" "∨")
	(pretty-regexp "TLog" "L")
	(pretty-regexp "LogUtil" "L")
	(pretty-regexp "Log" "L")
	(pretty-regexp "switch " "s")
	(pretty-regexp "case" "c")
	(pretty-regexp "default" "d")
	(pretty-regexp "break" "b")
	(pretty-regexp "else if " "l") ;; else if 
	(pretty-regexp "else " "e")
	(pretty-regexp "if " "i")
	(pretty-regexp ".equals" "=")
	(pretty-regexp "System.out.println" "L")
	(pretty-regexp " = " "←")
	(pretty-regexp " == " "≡")
	(pretty-regexp "Integer.valueOf" "I")
	(pretty-regexp "Float.valueOf" "F")
	(pretty-regexp "String.valueOf" "S")
	(pretty-regexp "Constants.EMPTY_STR" "\"")
	(pretty-regexp "PrefUtil.setKey" "⤋")
	(pretty-regexp "PrefUtil.getKeyBoolean" "⤊")
	(pretty-regexp "PrefUtil.getKeyString" "⤊")
	(pretty-regexp "NetEngine.getInst()" "≈")
	(pretty-regexp "String" "S")
	(pretty-regexp "intValue" "i")
	(pretty-regexp "int" "i")
	(pretty-regexp "booleanValue" "b")
	(pretty-regexp "boolean" "b")
	(pretty-regexp "long" "J")
	(pretty-regexp "floatValue" "f")
	(pretty-regexp "float" "f")
	(pretty-regexp "try " "T")
	(pretty-regexp "catch " "C")
	(pretty-regexp "catch" "C")
	(pretty-regexp "Exception" "E")
	(pretty-regexp "finally " "F") ;;finally
	(pretty-regexp "public static final" "Τ")
	(pretty-regexp "public final static" "Τ")
	(pretty-regexp "public static" "Τ")
	(pretty-regexp "private static final" "Υ")
	(pretty-regexp "private final static" "Υ")
	(pretty-regexp "private static" "Υ")
	(pretty-regexp "static" "s")
	(pretty-regexp "private void" "v")
	(pretty-regexp "private" "p")
	(pretty-regexp "protected void" "v")
	(pretty-regexp "public void" "V")
	(pretty-regexp "public" "P")
	(pretty-regexp "final" "f")
	(pretty-regexp "return" "r")))

;;;###autoload
(define-minor-mode pretty-mode+
  :group 'pretty
  :lighter " pretty+"
  (if pretty-mode
	  (load-extra-pretty)
	(font-lock-remove-keywords nil (pretty-keywords))
	(remove-text-properties (point-min) (point-max) '(composition nil))))

(add-hook 'pretty-mode-hook 'pretty-mode+)

(provide 'pretty-mode+)
