;*=====================================================================*/
;*    .../project/hop/2.5.x/scheme2js/constant_propagation.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  2007-11                                           */
;*    Last change :  Wed Sep  4 11:52:23 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Constant propagation                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module constant-propagation
   
   (import config
	   nodes
	   export-desc
	   walk
	   var-ref-util
	   side
	   use-count
	   verbose)
   
   (export (constant-propagation! tree::Module)))

;*---------------------------------------------------------------------*/
;*    constant-propagation! ...                                        */
;*---------------------------------------------------------------------*/
(define (constant-propagation! tree)
   (if (config 'constant-propagation)
       (unless (config 'call/cc)
	  (verbose "propagation")
	  (side-effect tree)
	  (propagate! tree #f))))

;*---------------------------------------------------------------------*/
;*    propagate! ::Node ...                                            */
;*---------------------------------------------------------------------*/
(define-nmethod (Node.propagate!)
   (default-walk! this))

;*---------------------------------------------------------------------*/
;*    transitive-value ...                                             */
;*---------------------------------------------------------------------*/
(define (transitive-value var-ref::Ref)
   (if (runtime-ref? var-ref)
       var-ref
       (with-access::Ref var-ref (var)
	  (with-access::Var var (constant? value)
	     (cond
		((and constant?
		      value
		      (isa? value Const)
		      (with-access::Const value ((const value))
			 ;; do not propagate vectors, lists and
			 ;; strings. Otherwise 'eq?' might not work anymore.
			 ;; Also strings can be quite long.
			 (or (number? const)  
			     (symbol? const)
			     (char? const)
			     (boolean? const)
			     (eqv? #unspecified const))))
		 value)
		((and constant?
		      value
		      (isa? value Ref)
		      (with-access::Ref value (var)
			 (with-access::Var var (constant? kind)
			    (and constant? (not (eq? kind 'this))))))
		 (transitive-value value))
		(else var-ref))))))

;*---------------------------------------------------------------------*/
;*    propagate! ::Ref ...                                             */
;*---------------------------------------------------------------------*/
(define-nmethod (Ref.propagate!)
   (let* ((target (transitive-value this)))
      (cond
	 ((isa? target Const)
	  (duplicate::Const target))
	 ((and (isa? target Ref)
	       (not (eq? this target)))
	  (with-access::Ref target (var)
	     (var-reference var :location target)))
	 (else this))))

;*---------------------------------------------------------------------*/
;*    propagate! ::Set! ...                                            */
;*---------------------------------------------------------------------*/
(define-nmethod (Set!.propagate!)
   ;; don't visit lvalue
   (with-access::Set! this (val)
      (set! val (walk! val)))
   this)
