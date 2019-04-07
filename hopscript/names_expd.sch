;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/names_expd.sch            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Apr  1 08:50:34 2019                          */
;*    Last change :  Sun Apr  7 06:13:08 2019 (serrano)                */
;*    Copyright   :  2019 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    HopScript name expanders                                         */
;*    -------------------------------------------------------------    */
;*    See expanders.sch and names.sch                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    js-&-expander ...                                                */
;*---------------------------------------------------------------------*/
(define (js-&-expander x e)
   (match-case x
      ((& (and ?val (? string?)))
       (cond
	  ((member val '("__proto__" "Array" "Buffer" "Error"
			 "GLOBAL" "Infinity" "-Infinity" "NaN" "Object"
			 "String" "Worker" "apply" "call" "callee" "caller"
			 "charAt" "charCodeAt"
			 "compiler" "configurable" "console"
			 "constructor" "done" "enumerable"
			 "exec" "exports" "false" "for" "forEach" "get" "global"
			 "hop" "indexOf" "init"
			 "iterator" "join" "keyFor" "lastIndex" "lastIndexOf"
			 "length" "localeCompare" "log" "match" "map" "module"
			 "name" "naturalCompare" "next" "null" "path"
			 "pop" "prototype" "push" "replace"
			 "require" "return" "set" "slice" "split" "substr"
			 "substring" "toLowerCase" "toLocaleLowerCase"
			 "toUpperCase"
			 "toLocaleUpperCase" "toString" "trim" "true"
			 "undefined" "value"
			 "valueOf" "writable"))
	   `(@ ,(symbol-append '& (string->symbol val)) __hopscript_names))
	  ((string=? val "")
	   '(@ &empty __hopscript_names))
	  ((string->number val)
	   =>
	   (lambda (n)
	      (if (fixnum? n)
		  (evepairify `(js-integer->jsstring ,n) x)
		  (evepairify `(js-ascii-name->jsstring ,val) x))))
	  ((eq? (string-minimal-charset val) 'ascii)
	   (tprint "ASCII " val)
	   (evepairify `(js-ascii-name->jsstring ,val) x))
	  (else
	   (evepairify `(js-utf8-name->jsstring ,val) x))))
      ((& (and ?val ((kwote quot) (? symbol?))))
       (error "&" "wrong syntax" x))
      ((& ?val)
       `(js-name->jsstring ,(e (evepairify val x) e)))
      (else
       (error "&" "wrong syntax" x))))
