;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/names.sch                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Nov 22 06:35:05 2014                          */
;*    Last change :  Sun Mar 31 10:18:49 2019 (serrano)                */
;*    Copyright   :  2014-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    names Helper macros.                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (import __hopscript_names))

;*---------------------------------------------------------------------*/
;*    js-name->jsstring                                                */
;*---------------------------------------------------------------------*/
(define-expander &
   (lambda (x e)
      (match-case x
	 ((& (and ?val (? string?)))
	  (cond
	     ((member val '("__proto__" "charAt" "charCodeAt" "compiler"
			    "configurable" "constructor" "done" "enumerable"
			    "exec" "for" "forEach" "indexOf" "iterator"
			    "keyFor" "lastIndex" "lastIndexOf" "length"
			    "localeCompare" "match" "map" "naturalCompare"
			    "next" "prototype" "replace" "return" "slice"
			    "split" "substr" "substring" "toLowerCase"
			    "toLocaleLowerCase" "toUpperCase"
			    "toLocaleUpperCase" "toString" "trim" "value"
			    "valueOf" "writable"))
	      `((@ ,(symbol-append '& (string->symbol val)) __hopscript_names)))
	     ((string=? val "")
	      '(@ &empty __hopscript_names))
	     ((eq? (string-minimal-charset val) 'ascii)
	      (evepairify `(js-ascii-name->jsstring ,val) x))
	     (else
	      (evepairify `(js-name-utf8->jsstring ,val) x))))
	 ((& (and ?val ((kwote quot) (? symbol?))))
	  (error "&" "wrong syntax" x))
	 ((& ?val)
	  `(js-name->jsstring ,(e (evepairify val x) e)))
	 (else
	  (error "&" "wrong syntax" x)))))

   
