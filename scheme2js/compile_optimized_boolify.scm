;*=====================================================================*/
;*    .../hop/2.5.x/scheme2js/compile_optimized_boolify.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  2007-11                                           */
;*    Last change :  Sun Aug 11 15:48:42 2013 (serrano)                */
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
   
   (export (compile-boolified p compile::procedure node::Node tmp)))

;*---------------------------------------------------------------------*/
;*    compile-optimized-if-boolify ...                                 */
;*---------------------------------------------------------------------*/
(define (compile-optimized-if-boolify p compile n tmp)
   (with-access::If n (test then else)
      (if (and (isa? else Const)
	       (with-access::Const else (value)
		  (not value)))
	  (template-display p
	     "(~e&&~e)"
	     (compile test p #f tmp #f)
	     (compile-optimized-boolify p compile then tmp))
	  (compile-unoptimized-boolify p compile n tmp))))

;*---------------------------------------------------------------------*/
;*    compile-optimized-boolify ...                                    */
;*---------------------------------------------------------------------*/
(define (compile-optimized-boolify p compile n tmp)
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
			(compile n p #f tmp #f)
			(compile-unoptimized-boolify p compile n tmp))))
	      (compile-unoptimized-boolify p compile n tmp))))
      ((isa? n If)
       (compile-optimized-if-boolify p compile n tmp))
      ((isa? n Const)
       (with-access::Const n (value)
	  (template-display p
	     "~a" (if value "true" "false"))))
      (else
       (compile-unoptimized-boolify p compile n tmp))))

;*---------------------------------------------------------------------*/
;*    compile-unoptimzed-boolify ...                                   */
;*---------------------------------------------------------------------*/
(define (compile-unoptimized-boolify p compile node tmp)
   (template-display p
      "(~e !== false)"
      (compile node p #f tmp #f)))

;*---------------------------------------------------------------------*/
;*    compile-boolified ...                                            */
;*---------------------------------------------------------------------*/
(define (compile-boolified p compile node tmp)
   ;; TODO: get rid of '(config ... )
   (if (config 'optimize-boolify)
       (compile-optimized-boolify p compile node tmp)
       (compile-unoptimized-boolify p compile node tmp)))

