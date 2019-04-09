;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/names_expd.sch            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Apr  1 08:50:34 2019                          */
;*    Last change :  Tue Apr  9 17:33:00 2019 (serrano)                */
;*    Copyright   :  2019 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    HopScript name expanders                                         */
;*    -------------------------------------------------------------    */
;*    See expanders.sch and names.sch                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    &name-expander ...                                               */
;*---------------------------------------------------------------------*/
(define (&name-expander x e)
   (match-case x
      ((?- (and ?val (? string?)))
       (cond
	  ((member val '("__proto__"
			 "GLOBAL"
			 "apply"
			 "exports"
			 "get"
			 "global"
			 "length"
			 "prototype"
			 "readable"
			 "require"
			 "set"
			 "toString"
			 "value"
			 "write"
			 "writable"))
;* 			 "Array" "Buffer" "Error"                      */
;* 			 "GLOBAL" "Infinity" "-Infinity" "NaN" "Object" */
;* 			 "String" "Worker" "apply" "call" "callee" "caller" */
;* 			 "charAt" "charCodeAt"                         */
;* 			 "compiler" "configurable" "console"           */
;* 			 "constructor" "done" "enumerable"             */
;* 			 "exec" "exports" "false" "for" "forEach" "get" "global" */
;* 			 "hop" "indexOf" "init"                        */
;* 			 "iterator" "join" "keyFor" "lastIndex" "lastIndexOf" */
;* 			 "length" "localeCompare" "log" "match" "map" "module" */
;* 			 "name" "naturalCompare" "next" "null" "path"  */
;* 			 "pop" "prototype" "push" "replace"            */
;* 			 "require" "return" "set" "slice" "split" "substr" */
;* 			 "substring" "toLowerCase" "toLocaleLowerCase" */
;* 			 "toUpperCase"                                 */
;* 			 "toLocaleUpperCase" "toString" "trim" "true"  */
;* 			 "undefined" "value"                           */
;* 			 "valueOf" "writable"))                        */
	   `(@ ,(symbol-append '& (string->symbol val)) __hopscript_names))
	  ((string=? val "")
	   '(@ &empty __hopscript_names))
	  ((string->number val)
	   =>
	   (lambda (n)
	      (if (fixnum? n)
		  (vector 2 n)
		  (vector 0 val))))
	  ((eq? (string-minimal-charset val) 'ascii)
	   (vector 0 val))
	  (else
	   (vector 1 val))))
      (else
       (error "&name" "bad syntax" x))))
