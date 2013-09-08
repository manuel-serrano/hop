;*=====================================================================*/
;*    .../hop/2.5.x/scheme2js/compile_optimized_boolify.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  2007-11                                           */
;*    Last change :  Thu Sep  5 16:05:44 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Conditional expression compilation                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module compile-optimized-boolify
   
   (import config
	   tools
	   nodes
	   export-desc
	   template-display
	   verbose)
   
   (export (compile-boolified p compile::procedure node::Node)))

;*---------------------------------------------------------------------*/
;*    compile-optimized-if-boolify ...                                 */
;*---------------------------------------------------------------------*/
(define (compile-optimized-if-boolify p compile n)
   (with-access::If n (test then else)
      (if (and (isa? else Const)
	       (with-access::Const else (value)
		  (not value)))
	  (template-display p
	     "(~e&&~e)"
	     (compile test p #f #f)
	     (compile-optimized-boolify p compile then))
	  (compile-unoptimized-boolify p compile n))))

;*---------------------------------------------------------------------*/
;*    compile-optimized-boolify ...                                    */
;*---------------------------------------------------------------------*/
(define (compile-optimized-boolify p compile n)
   (cond
      ((isa? n Call)
       (with-access::Call n (operator operands)
	  (if (isa? operator Ref)
	      (with-access::Ref operator (var)
		 (with-access::Var var (kind constant? export-desc)
		    (if (and (or (eq? kind 'exported) (eq? kind 'imported))
			     constant?
			     (eq? (with-access::Export-Desc export-desc
					(return-type) return-type)
				'bool))
			(compile n p #f #f)
			(compile-unoptimized-boolify p compile n))))
	      (compile-unoptimized-boolify p compile n))))
      ((isa? n If)
       (compile-optimized-if-boolify p compile n))
      ((isa? n Const)
       (with-access::Const n (value)
	  (template-display p
	     "~a" (if value "true" "false"))))
      (else
       (compile-unoptimized-boolify p compile n))))

;*---------------------------------------------------------------------*/
;*    compile-unoptimzed-boolify ...                                   */
;*---------------------------------------------------------------------*/
(define (compile-unoptimized-boolify p compile node)
   (template-display p
      "(~e !== false)"
      (compile node p #f #f)))

;*---------------------------------------------------------------------*/
;*    compile-boolified ...                                            */
;*---------------------------------------------------------------------*/
(define (compile-boolified p compile node)
   ;; TODO: get rid of '(config ... )
   (if (config 'optimize-boolify)
       (compile-optimized-boolify p compile node)
       (compile-unoptimized-boolify p compile node)))

