;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/scheme2js/tail_rec.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  2007-13                                           */
;*    Last change :  Mon Aug 19 08:31:48 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Tail-rec optimization                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module tail-rec
   
   (import config
	   tools
	   nodes
	   export-desc
	   walk
	   symbol
	   var-ref-util
	   transform-util
	   side
	   tail
	   loop-updates
	   captured-vars
	   verbose)
   
   (static (wide-class Tail-Lambda::Lambda
	      label::Label)
	   (wide-class Repl-Var::Var
	      replacement::Ref))
   
   (export (tail-rec! tree::Module)))

;*---------------------------------------------------------------------*/
;*    tail-rec ...                                                     */
;*    -------------------------------------------------------------    */
;*    (define (foo x y z)                                              */
;*        bar                                                          */
;*        (foo x_up y_up z_up))                                        */
;*                                                                     */
;*    is transformed into:                                             */
;*                                                                     */
;*    (define (foo x_ y_ z_)                                           */
;*       (tail-rec (x y z)                                             */
;*                 (x_ y_ z_))                                         */
;*          bar                                                        */
;*          (tail-rec-call (x_up y_up z_up))))                         */
;*                                                                     */
;*    The meaning of tail-rec being:                                   */
;*      (let loop ((x x_) (y y_) (z z_))                               */
;*    The meaning of tail-rec-call:                                    */
;*      (loop x_up y_up z_up)                                          */
;*---------------------------------------------------------------------*/
(define (tail-rec! tree)
   (verbose "tail-rec")
   (tail-calls tree)
   (when (config 'optimize-tail-rec)
      (side-effect tree)
      (rec! tree #f #f)))

;*---------------------------------------------------------------------*/
;*    rec! ::Node ...                                                  */
;*---------------------------------------------------------------------*/
(define-nmethod (Node.rec! current-fun)
   (default-walk! this current-fun))

;*---------------------------------------------------------------------*/
;*    rec! ::Tail-Call ...                                             */
;*---------------------------------------------------------------------*/
(define-nmethod (Tail-Call.rec! current-fun)
   (default-walk! this current-fun)
   (with-access::Call this (operator operands)
      (if (and current-fun
	       (isa? operator Ref)
	       (with-access::Ref operator (var)
		  (with-access::Var var (constant? value)
		     (and constant?
			  (eq? value current-fun)))))
	  (with-access::Lambda current-fun (formals vaarg?)
	     (with-access::Ref operator (var)
		(with-access::Var var (id)
		   (let* ((assig-mapping (parameter-assig-mapping id
							    this
							    operands
							    formals
							    vaarg?))
			  (updates (map! cdr assig-mapping)))
		      (unless (isa? current-fun Tail-Lambda)
			 (widen!::Tail-Lambda current-fun
			    (label (instantiate::Label
				      (id (gensym 'continue))))))
		      (instantiate::Tail-rec-Call
			 (updates updates)
			 (label (with-access::Tail-Lambda current-fun (label)
				   label)))))))
	  this)))

;*---------------------------------------------------------------------*/
;*    rec! ::Lambda ...                                                */
;*---------------------------------------------------------------------*/
(define-nmethod (Lambda.rec! current-fun)
   (default-walk! this this)
   (when (isa? this Tail-Lambda)
      (with-access::Tail-Lambda this (label body formals scope-vars)
	 ;; replace formals with replacement variables, and replace body
	 ;; with while.
	 (let* ((return-val (with-access::Return body (val) val))
		;; body must be a Return
		(formals-vars (map (lambda (f)
				      (with-access::Ref f (var) var))
				 formals))
		(replacement-decls (map (lambda (formal)
					   (with-access::Ref formal (var id)
					      (let ((decl (Ref-of-new-Var id)))
						 (widen!::Repl-Var var
						    (replacement decl))
						 decl)))
				      formals))
		(replacement-vars (map (lambda (r)
					  (with-access::Ref r (var) var))
				     replacement-decls))
		(inits (map (lambda (formal repl-var)
			       (instantiate::Set!
				  (location -50)
				  (lvalue formal)
				  (val (var-reference repl-var
					  :location formal))))
			  formals
			  replacement-vars))
		(tail-rec (instantiate::Tail-rec
			     (scope-vars formals-vars)
			     (inits inits)
			     (body return-val)
			     (label label))))
	    
	    (set! scope-vars
	       (map! (lambda (var)
			(if (isa? var Repl-Var)
			    (with-access::Repl-Var var (replacement)
			       (with-access::Ref replacement (var)
				  var))
			    var))
		  scope-vars))
	    (set! formals (map (lambda (formal)
				  (with-access::Ref formal (var)
				     (with-access::Repl-Var var (replacement)
					(let ((tmp replacement))
					   (shrink! var)
					   tmp))))
			     formals))
	    (with-access::Return body (val)
	       (set! val tail-rec))
	    (shrink! this))))
   this)
