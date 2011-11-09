;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-11 Florian Loitsch, see LICENSE file         */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module side
   (import config
	   error
	   nodes
	   export-desc
	   walk
	   verbose)
   (static (class Env
	      runtime-is-constant?::bool))
   (export (side-effect tree::Module)))

(define (side-effect tree)
   (verbose "side-effect")
   (side tree (instantiate::Env (runtime-is-constant? (config 'runtime-is-constant)))))

(define-nmethod (Node.side)
   (default-walk this))

(define-nmethod (Module.side)
   (with-access::Module this (runtime-vars imported-vars scope-vars)
      (for-each (lambda (js-var)
		   (with-access::Var js-var (already-defined? constant? value)
		      (set! already-defined? #t)
		      (set! constant? (with-access::Env env (runtime-is-constant?) runtime-is-constant?))
		      (set! value #f)))
		runtime-vars)
      (for-each (lambda (js-var)
		   (with-access::Var js-var
			 (export-desc already-defined? constant? value)
		      (with-access::Export-Desc export-desc (exported-as-const?)
			 (set! already-defined? #t)
			 (set! constant? exported-as-const?)
			 (set! value #f))))
		imported-vars)
      (for-each (lambda (js-var)
		   (with-access::Var js-var
			 (export-desc already-defined? constant? value)
		      (with-access::Export-Desc export-desc (exported-as-const?)
			 (set! already-defined? (not exported-as-const?))
			 (set! constant? #f)
			 (set! value #f))))
		scope-vars)
      (default-walk this)))

(define-nmethod (Lambda.side)
   (with-access::Lambda this (scope-vars)
      (for-each (lambda (var)
		   (with-access::Var var (already-defined? constant? value)
		      (set! already-defined? #t)
		      (set! constant? #t)
		      (set! value #f)))
		scope-vars))
   ;; revisits the formals, but doesn't make any difference.
   (default-walk this))

(define (clean-local l::Var)
   (with-access::Var l (already-defined? constant? value)
      (set! already-defined? #f)
      (set! constant? #f)
      (set! value #f)))
   
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

(define-nmethod (Scope.side)
   (with-access::Scope this (scope-vars)
      (for-each clean-local scope-vars))
   (default-walk this))

(define-nmethod (Set!.side)
   (with-access::Set! this (lvalue val)
      (walk val)
      (with-access::Ref lvalue (var)
	 (when (and (eq? (Var-kind var) 'imported)
		    (Var-constant? var)) ;; equal to exported-as-const?
	    (scheme2js-error
	     "Set!"
	     "Imported variable is constant, and must not be modified."
	     (Var-id var)
	     lvalue))
	 (with-access::Var var (already-defined? constant? value)
	    (if already-defined?
		(begin
		   (set! constant? #f)
		   (set! value #f))
		(begin
		   (set! already-defined? #t)
		   (set! constant? #t)
		   (set! value val)))))))
