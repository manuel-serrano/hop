;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/scheme2js/side.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  2007-13                                           */
;*    Last change :  Sun Aug 11 16:48:11 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Side effects                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module side
   
   (import config
	   error
	   nodes
	   export-desc
	   dump-node
	   walk
	   verbose)
   
   (static (class Side-Env
	      runtime-is-constant?::bool
	      (pass::int read-only))

	   (wide-class DefinedVar::Var
	      (pass::int (default -1))))
   
   (export (side-effect tree::Module)))

;*---------------------------------------------------------------------*/
;*    side-effect ...                                                  */
;*---------------------------------------------------------------------*/
(define (side-effect tree)
   (verbose "side-effect")
   (set! *pass-index* (+fx 1 *pass-index*))
   (side tree
      (instantiate::Side-Env
	 (runtime-is-constant? (config 'runtime-is-constant))
	 (pass *pass-index*))))

;*---------------------------------------------------------------------*/
;*    *pass-index* ...                                                 */
;*---------------------------------------------------------------------*/
(define *pass-index* 0)

;*---------------------------------------------------------------------*/
;*    side ::Node ...                                                  */
;*---------------------------------------------------------------------*/
(define-nmethod (Node.side)
   (default-walk this))

;*---------------------------------------------------------------------*/
;*    side ::Module ...                                                */
;*---------------------------------------------------------------------*/
(define-nmethod (Module.side)
   (with-access::Module this (runtime-vars imported-vars scope-vars)
      (for-each (lambda (js-var)
		   (with-access::Var js-var (constant? value)
		      (with-access::Side-Env env (runtime-is-constant? pass)
			 (widen!::DefinedVar js-var
			    (pass pass))
			 (set! constant? runtime-is-constant?))
		      (set! value #f)))
	 runtime-vars)
      (for-each (lambda (js-var)
		   (with-access::Var js-var
			 (export-desc constant? value)
		      (with-access::Side-Env env (pass)
			 (widen!::DefinedVar js-var
			    (pass pass))
			 (with-access::Export-Desc export-desc (exported-as-const?)
			    (set! constant? exported-as-const?)
			    (set! value #f)))))
	 imported-vars)
      (for-each (lambda (js-var)
		   (with-access::Var js-var
			 (export-desc constant? value id)
		      (with-access::Side-Env env (pass)
			 (widen!::DefinedVar js-var
			    (pass (-fx pass 1))))
		      (with-access::Export-Desc export-desc (exported-as-const?)
			 (set! constant? exported-as-const?)
			 (set! value #f))))
	 scope-vars)
      (default-walk this)))

;*---------------------------------------------------------------------*/
;*    side ::Lambda ...                                                */
;*---------------------------------------------------------------------*/
(define-nmethod (Lambda.side)
   (with-access::Lambda this (scope-vars)
      (for-each (lambda (var)
		   (with-access::Var var (constant? value)
		      (with-access::Side-Env env (pass)
			 (widen!::DefinedVar var
			    (pass pass)))
		      (set! constant? #t)
		      (set! value #f)))
	 scope-vars))
   ;; revisits the formals, but doesn't make any difference.
   (default-walk this))

;*---------------------------------------------------------------------*/
;*    clean-local ...                                                  */
;*---------------------------------------------------------------------*/
(define (clean-local l::Var)
   (when (isa? l DefinedVar)
      (shrink! l))
   (with-access::Var l (constant? value)
      (set! constant? #f)
      (set! value #f)))

;*---------------------------------------------------------------------*/
;*    side ::Tail-rec ...                                              */
;*---------------------------------------------------------------------*/
(define-nmethod (Tail-rec.side)
   (with-access::Tail-rec this (inits scope-vars body)
      (for-each clean-local scope-vars)
      
      (for-each walk inits)
      ;; we can leave the "constant?" flag, but we have to remove the
      ;; value-entry. Otherwise we might propagate the init-value.
      (for-each (lambda (var)
		   (with-access::Var var (value)
		      (set! value #f)))
	 scope-vars)
      (walk body)))

;*---------------------------------------------------------------------*/
;*    side ::While ...                                                 */
;*---------------------------------------------------------------------*/
(define-nmethod (While.side)
   (with-access::While this (init scope-vars body)
      (for-each clean-local scope-vars)

      (walk init)
      ;; we can leave the "constant?" flag, but we have to remove the
      ;; value-entry. Otherwise we might propagate the init-value.
      (for-each (lambda (var)
		   (with-access::Var var (value)
		      (set! value #f)))
		scope-vars)
      (walk body)))

;*---------------------------------------------------------------------*/
;*    side ::Scope ...                                                 */
;*---------------------------------------------------------------------*/
(define-nmethod (Scope.side)
   (with-access::Scope this (scope-vars)
      (for-each clean-local scope-vars))
   (default-walk this))

;*---------------------------------------------------------------------*/
;*    side ::Set! ...                                                  */
;*---------------------------------------------------------------------*/
(define-nmethod (Set!.side)
   (with-access::Set! this (lvalue val)
      (walk val)
      (with-access::Ref lvalue (var)
	 (with-access::Var var (kind constant?)
	    (when (and (eq? kind 'imported) constant?)
	       ;; equal to exported-as-const?
	       (scheme2js-error
		  "Set!"
		  "Imported variable is constant, and must not be modified."
		  (with-access::Var var (id) id)
		  lvalue))
	    (with-access::Var var (constant? value)
	       (with-access::Side-Env env (pass)
		  (if (and (isa? var DefinedVar)
			   (with-access::DefinedVar var ((vpass pass))
			      (=fx vpass pass)))
		      (begin
			 (set! constant? #f)
			 (set! value #f))
		      (begin
			 (widen!::DefinedVar var
			    (pass pass))
			 (set! constant? #t)
			 (set! value val)))))))))
