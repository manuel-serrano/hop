;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-12 Florian Loitsch, see LICENSE file         */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module gen-js
   (export (mangle-JS-sym::bstring sym::symbol)
	   (gen-JS-sym::bstring sym::symbol)
	   (valid-JS-str?::bool str::bstring)
	   (mangle-qualified-var::bstring sym::symbol qualifier)))

(define counter 0)

(define *js-counter-mutex* (make-mutex))

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

;; mangle variables, so they are valid JS-vars.
(define (mangle-JS-sym sym)
   (let ((s (symbol->string sym)))
      (if (valid-JS-str? s)
	  s
	  (bigloo-mangle s))))

;; kind of adapted gen-sym
(define (gen-JS-sym sym)
   (synchronize *js-counter-mutex*
      (set! counter (+ counter 1))
      (mangle-JS-sym
	 (symbol-append 'sc_ ;; start with "sc_"
	    sym
	    '_
	    (string->symbol (integer->string counter))))))

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

(define (valid-JS-str? str)
   (not (or (bigloo-need-mangling? str)
	    (member str *reserved-js*)
	    ;; avoid clashes with runtime :
	    (string-prefix? "sc_" str)
	    (string-prefix? "SC_" str))))
