;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/stringliteral.sch         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Nov 22 06:35:05 2014                          */
;*    Last change :  Sat Mar 30 09:02:00 2019 (serrano)                */
;*    Copyright   :  2014-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JsStringLiteral Helper macros.                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (import __hopscript_stringliteral))

;*---------------------------------------------------------------------*/
;*    js-string->jsstring                                              */
;*---------------------------------------------------------------------*/
(define-expander js-string->jsstring
   (lambda (x e)
      (match-case x
	 ((js-string->jsstring (and ?val (? string?)))
	  (if (eq? (string-minimal-charset val) 'ascii)
	      (evepairify `(js-ascii->jsstring ,val) x)
	      (evepairify `(js-utf8->jsstring ,val) x)))
	 ((js-string->jsstring ?val)
	  `(js-string->jsstring ,(e (evepairify val x) e)))
	 (else
	  (error "js-string->jsstring" "wrong syntax" x)))))

;*---------------------------------------------------------------------*/
;*    js-name->jsstring                                                */
;*---------------------------------------------------------------------*/
(define-expander &
   (let ((multiples '()))
      (lambda (x e)
	 
	 (define (check-multiple val)
	    (if (member val multiples)
		(warning "&" "multiple define" val)
		(set! multiples (cons val multiples))))
	 
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
		 `((@ ,(symbol-append '& (string->symbol)) __hopscript_names)))
		((eq? (string-minimal-charset val) 'ascii)
		 (check-multiple val)
		 (evepairify `(js-ascii-name->jsstring ,val) x))
		(else
		 (check-multiple val)
		 (evepairify `(js-name-utf8->jsstring ,val) x))))
	    ((& (and ?val ((kwote quot) (? symbol?))))
	     (error "&" "wrong syntax" x))
	    ((& ?val)
	     `(js-name->jsstring ,(e (evepairify val x) e)))
	    (else
	     (error "&" "wrong syntax" x))))))

   
