;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/scheme2js/gen_js.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  2007-13                                           */
;*    Last change :  Thu Jul 25 16:02:41 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    JS names                                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module gen-js
   (export (mangle-JS-sym::bstring sym::symbol)
	   (gen-JS-sym::bstring sym::symbol)
	   (valid-JS-str?::bool str::bstring)
	   (mangle-qualified-var::bstring sym::symbol qualifier)
	   (JS-stamp::bstring)))

;*---------------------------------------------------------------------*/
;*    counter ...                                                      */
;*---------------------------------------------------------------------*/
(define counter 0)

;*---------------------------------------------------------------------*/
;*    *js-counter-mutex* ...                                           */
;*---------------------------------------------------------------------*/
(define *js-counter-mutex* (make-mutex))

;*---------------------------------------------------------------------*/
;*    mangle-qualified-var ...                                         */
;*---------------------------------------------------------------------*/
(define (mangle-qualified-var sym qualifier)
   (if (not qualifier)
       (mangle-JS-sym sym)
       (let* ((sym-str (symbol->string sym))
	      (qual-str (symbol->string qualifier)))
	  ;; MS: otherwise client-side runtime fails at demangling
	  ;; on error (see share/hop-exection.scm)
	  (bigloo-module-mangle sym-str qual-str))))
;* 	  (cond                                                        */
;* 	     ((and (bigloo-need-mangling? sym-str)                     */
;* 		   (bigloo-need-mangling? qual-str))                   */
;* 	      (bigloo-mangle                                           */
;* 	       (string-append (symbol->string sym)                     */
;* 			      "__"                                     */
;* 			      (symbol->string qualifier))))            */
;* 	     (else                                                     */
;* 	      (let ((mangled-sym (if (bigloo-need-mangling? sym-str)   */
;* 				     (bigloo-mangle sym-str)           */
;* 				     sym-str))                         */
;* 		    (mangled-qual (if (bigloo-need-mangling? qual-str) */
;* 				      (bigloo-mangle qual-str)         */
;* 				      qual-str)))                      */
;* 		 (string-append mangled-sym                            */
;* 				"__"                                   */
;* 				mangled-qual)))))))                    */

;*---------------------------------------------------------------------*/
;*    mangle-JS-sym ...                                                */
;*    -------------------------------------------------------------    */
;*    mangle variables, so they are valid JS-vars.                     */
;*---------------------------------------------------------------------*/
(define (mangle-JS-sym sym)
   (let ((s (symbol->string sym)))
      (if (valid-JS-str? s)
	  s
	  (bigloo-mangle s))))

;*---------------------------------------------------------------------*/
;*    gen-JS-sym ...                                                   */
;*    -------------------------------------------------------------    */
;*    kind of adapted gen-sym                                          */
;*---------------------------------------------------------------------*/
(define (gen-JS-sym sym)
   (synchronize *js-counter-mutex*
      (set! counter (+ counter 1))
      (mangle-JS-sym
	 (symbol-append 'sc_ ;; start with "sc_"
	    sym
	    '_
	    (string->symbol (integer->string counter))))))

;*---------------------------------------------------------------------*/
;*    JS-stamp ...                                                     */
;*---------------------------------------------------------------------*/
(define (JS-stamp)
   (synchronize *js-counter-mutex*
      (set! counter (+ counter 1))
      (integer->string counter)))
   
;*---------------------------------------------------------------------*/
;*    *reserved-js* ...                                                */
;*---------------------------------------------------------------------*/
(define *reserved-js*
   '("as" "break" "case" "catch" "class" "const" "continue" "default"
     "delete" "do" "else" "extends" "false" "finally" "for"
     "function" "if" "import" "in" "instanceof" "is" "namespace"
     "new" "null" "package" "private" "public" "return" "super"
     "switch" "this" "throw" "true" "try" "typeof" "use" "var"
     "void" "while" "with" "abstract" "debugger" "enum" "export"
     "goto" "implements" "interface" "native" "protected"
     "synchronized" "throws" "transient" "volatile"
     ))

;*---------------------------------------------------------------------*/
;*    valid-JS-str? ...                                                */
;*---------------------------------------------------------------------*/
(define (valid-JS-str? str)
   (not (or (bigloo-need-mangling? str)
	    (member str *reserved-js*)
	    ;; avoid clashes with runtime :
	    (string-prefix? "sc_" str)
	    (string-prefix? "SC_" str))))
