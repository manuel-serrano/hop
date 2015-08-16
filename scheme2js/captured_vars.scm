;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/scheme2js/captured_vars.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  2007-13                                           */
;*    Last change :  Tue Jul 30 07:14:15 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Free/bound variables property                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module captured-vars
   
   (import nodes
	   dump-node
	   export-desc
	   walk
	   free-vars
	   side
	   verbose)
   
   (static (class Capture-Env
	      token::symbol)
	   (wide-class Capture-Lambda::Lambda
	      token::symbol))
   
   (export (captured-vars tree::Module)))

;*---------------------------------------------------------------------*/
;*    captured-vars ...                                                */
;*    -------------------------------------------------------------    */
;*    if a function captures a variable, it is marked as ".closure?".  */
;*    Every variable that is captured is marked as ".captured?".       */
;*                                                                     */
;*    we handle some cases, where the function is only used within the */
;*    scope of its variables. That is, if a function has free          */
;*    variables, but the lifetime of the function itself is shorter    */
;*    than those of its free variables, then the function is not       */
;*    considered to be a closure. (and the function is not marked es   */
;*    .closure, nor are its free variables marked as .captured?.       */
;*---------------------------------------------------------------------*/
(define (captured-vars tree)
   (verbose " collect captured")
   (free-vars tree)
   (side-effect tree)
   (captured tree (instantiate::Capture-Env (token (gensym 'token)))))

;*---------------------------------------------------------------------*/
;*    clean-var ...                                                    */
;*---------------------------------------------------------------------*/
(define (clean-var v::Var)
   (with-access::Var v (captured? id)
      (set! captured? #f)))

;*---------------------------------------------------------------------*/
;*    clean-lambda ...                                                 */
;*    -------------------------------------------------------------    */
;*    cleans lambda, if it's the first time we encounter the lambda.   */
;*---------------------------------------------------------------------*/
(define (clean-lambda l::Lambda env)
   (with-access::Capture-Env env (token)
      (unless (and (isa? l Capture-Lambda)
		   (eq? (with-access::Capture-Lambda l (token) token) token))
	 (widen!::Capture-Lambda l (token token))
	 (with-access::Lambda l (closure? scope-vars)
	    (set! closure? #f)
	    ;; this-var cannot leave local scope and can thus not be captured
	    (for-each clean-var scope-vars)))))

;*---------------------------------------------------------------------*/
;*    mark-closure! ...                                                */
;*---------------------------------------------------------------------*/
(define (mark-closure! proc)
   (with-access::Lambda proc (closure? free-vars)
      (unless closure? ;; already done
	 (set! closure? #t)
	 (for-each (lambda (var)
		      (with-access::Var var (captured?)
			 (set! captured? #t)))
		   free-vars))))

;*---------------------------------------------------------------------*/
;*    captured ::Node ...                                              */
;*---------------------------------------------------------------------*/
(define-nmethod (Node.captured)
   (default-walk this))

;*---------------------------------------------------------------------*/
;*    Lambda-non-closure-walk ...                                      */
;*---------------------------------------------------------------------*/
(define (Lambda-non-closure-walk l::Lambda env)
   (clean-lambda l env)
   (walk0 l env captured))

;*---------------------------------------------------------------------*/
;*    captured ::Lambda ...                                            */
;*---------------------------------------------------------------------*/
(define-nmethod (Lambda.captured)
   (clean-lambda this env)
   (with-access::Lambda this (free-vars)
      (when (not (null? free-vars))
	 (mark-closure! this))
      (default-walk this)))

;*---------------------------------------------------------------------*/
;*    captured ::Set! ...                                              */
;*---------------------------------------------------------------------*/
(define-nmethod (Set!.captured)
   (with-access::Set! this (lvalue val)
      (with-access::Ref lvalue (var)
	 ;; If val is a lambda do not yet mark it as closure (if it has free
	 ;; vars), but wait for its first use. (In the best case we are able to
	 ;; determine that all free vars are still alive.
	 (if (and (with-access::Var var (constant?) constant?)
		  (isa? val Lambda))
	     (Lambda-non-closure-walk val env)
	     (walk val)))))

;*---------------------------------------------------------------------*/
;*    captured ::Module ...                                            */
;*    -------------------------------------------------------------    */
;*    a Call is the only place, where we allow capturing functions.    */
;*---------------------------------------------------------------------*/
(define-nmethod (Call.captured)
   (with-access::Call this (operator operands)
      (cond
	 ((isa? operator Lambda)
	  (Lambda-non-closure-walk operator env)) ;; NOT (walk operator).
	 ((isa? operator Ref)
	  ;; no need to go into Ref. if it references a lambda, we don't
	  ;; want to know (as we allow lambda-refs in calls).
	  'done)
	 (else
	  (walk operator)))
      (for-each walk operands)))

;*---------------------------------------------------------------------*/
;*    captured ::Module ...                                            */
;*---------------------------------------------------------------------*/
(define-nmethod (Module.captured)
   (with-access::Module this (scope-vars)
      ;; don't care for runtime and imported variables.
      (for-each (lambda (v)
		   (with-access::Var v (captured?)
		      (set! captured? #t)))
	 scope-vars))
   (default-walk this))

;*---------------------------------------------------------------------*/
;*    captured ::Scope ...                                             */
;*---------------------------------------------------------------------*/
(define-nmethod (Scope.captured)
   (with-access::Scope this (scope-vars)
      (for-each clean-var scope-vars))
   (default-walk this))

;*---------------------------------------------------------------------*/
;*    captured ::Frame-alloc ...                                       */
;*---------------------------------------------------------------------*/
(define-nmethod (Frame-alloc.captured)
   (default-walk this)
   (with-access::Frame-alloc this (storage-var)
      (with-access::Var storage-var (captured?)
	 (set! captured? #t))))

;*---------------------------------------------------------------------*/
;*    captured ::Ref ...                                               */
;*---------------------------------------------------------------------*/
(define-nmethod (Ref.captured)
   (with-access::Ref this (var)
      (with-access::Var var (constant? value)
	 (if (and constant?
		  (isa? value Lambda))
	     (mark-closure! value)))))
