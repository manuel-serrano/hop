;*=====================================================================*/
;*    .../project/hop/2.5.x/scheme2js/compile_optimized_set.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  2007-11                                           */
;*    Last change :  Mon Jul 22 09:13:37 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Compile set! expression                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module compile-optimized-set
   
   (import config
	   tools
	   template-display
	   nodes
	   allocate-names
	   export-desc)
   
   (export (compile-unoptimized-set! p compile::procedure n::Node tmp)
	   (compile-set! p compile::procedure node::Node tmp)))

;*---------------------------------------------------------------------*/
;*    *set!-operators*                                                 */
;*---------------------------------------------------------------------*/
(define *set!-operators*
   '(("sc_plus" "+")
     ("sc_multi" "*")
     ("sc_minus" "-")
     ("sc_div" "/")
     ;; TODO: modulo can't be converted directly to '%'
     ("sc_modulo" "%")
     ("sc_bitAnd" "&")
     ("sc_bitOr" "|")
     ("sc_bitXor" "^")))

;*---------------------------------------------------------------------*/
;*    compile-optimized-set! ...                                       */
;*---------------------------------------------------------------------*/
(define (compile-optimized-set! p compile n tmp)
   (with-access::Set! n (lvalue val)
      (with-access::Ref lvalue (var)
	 (if (isa? val Call)
	     (with-access::Call val (operator operands)
		(if (and (not (null? operands))
			 (not (null? (cdr operands)))
			 ;; next test not strictly necessary, but
			 ;; simplifies cases like "(set! x (- x 1 2 3))"
			 (null? (cddr operands))
			 (isa? (car operands) Ref)
			 (eq? (with-access::Ref (car operands) (var) var) var)
			 (isa? operator Ref)
			 (let ((op-var (with-access::Ref operator (var) var)))
			    (with-access::Var op-var (constant? kind)
			       (and (eq? kind 'imported) constant?))))
		    (let* ((op-var (with-access::Ref operator (var) var))
			   (desc (with-access::Var op-var (export-desc) export-desc))
			   (js-id (with-access::Export-Desc desc (js-id) js-id))
			   (entry (assoc js-id *set!-operators*)))
		       (if entry
			   ;; get ++ and --
			   (if (and (or (string=? (cadr entry) "+")
					(string=? (cadr entry) "-"))
				    (isa? (cadr operands) Const)
				    (eq? 1 (with-access::Const (cadr operands) (value) value)))
			       (template-display p
				  "(~a~a~a)"
				  (cadr entry) (cadr entry) ;; ++ or --
				  (with-access::Named-Var var (js-id) js-id))
			       (with-access::Named-Var var (js-id)
				  (template-display p
				     "($js-id ~a= ~e)"
				     (cadr entry)
				     (compile (cadr operands) p #f tmp))))
			   (compile-unoptimized-set! p compile n tmp)))
		    (compile-unoptimized-set! p compile n tmp)))
	     (compile-unoptimized-set! p compile n tmp)))))

;*---------------------------------------------------------------------*/
;*    compile-unoptimized-set! ...                                     */
;*---------------------------------------------------------------------*/
(define (compile-unoptimized-set! p compile n tmp)
   (with-access::Set! n (lvalue val)
      (template-display p
	 "~e = ~e"
	 (compile lvalue p #f tmp)
	 (compile val p #f tmp))))

;*---------------------------------------------------------------------*/
;*    compile-set! ...                                                 */
;*---------------------------------------------------------------------*/
(define (compile-set! p compile n tmp)
   ;; TODO: get rid of '(config ...)
   (if (config 'optimize-set!)
       (compile-optimized-set! p compile n tmp)
       (compile-unoptimized-set! p compile n tmp)))
