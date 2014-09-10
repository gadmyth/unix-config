(require 'pretty-mode)


(defun load-extra-pretty ()
  (when (eq major-mode 'java-mode)
	(pretty-regexp "import" "I")
	(pretty-regexp "Boolean.FALSE" "×")
	(pretty-regexp "true" "✓")
	(pretty-regexp "Boolean.TRUE" "✓")
	(pretty-regexp "TextUtils.isEmpty" "⌘")
	(pretty-regexp "!" "¬")
	(pretty-regexp "&&" "∧")
	(pretty-regexp "||" "∨")
	(pretty-regexp "TLog" "L")
	(pretty-regexp "Log" "L")
	(pretty-regexp "LogUtil" "L")
	(pretty-regexp "System.out.println" "L")
	(pretty-regexp " = " "←")
	(pretty-regexp " == " "≡")
	(pretty-regexp "Constants.EMPTY_STR" "\"")
	(pretty-regexp "PrefUtil.setKey" "⤋")
	(pretty-regexp "PrefUtil.getKeyBoolean" "⤊")
	(pretty-regexp "PrefUtil.getKeyString" "⤊")
	(pretty-regexp "NetEngine.getInst()" "≈")
	(pretty-regexp "String" "S")
	(pretty-regexp "int" "I")
	(pretty-regexp "boolean" "B")
	(pretty-regexp "long" "J")
	(pretty-regexp "float" "f")
	(pretty-regexp "public static final" "Τ")
	(pretty-regexp "public final static" "Τ")
	(pretty-regexp "public static" "Τ")
	(pretty-regexp "private static final" "Υ")
	(pretty-regexp "private final static" "Υ")
	(pretty-regexp "private static" "Υ")
	(pretty-regexp "private void" "v")
	(pretty-regexp "protected void" "v")
	(pretty-regexp "public void" "V")))

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
