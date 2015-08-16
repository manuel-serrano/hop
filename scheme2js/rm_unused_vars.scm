;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/scheme2js/rm_unused_vars.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  2007-11                                           */
;*    Last change :  Thu Jul 25 09:46:10 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Unused variable elimination                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module rm-unused-vars
   
   (import nodes
	   export-desc
	   tools
	   walk
	   use-count
	   verbose)
   
   (static (wide-class Rm-Var::Var))
   
   (export (rm-unused-vars! tree::Module)))

;*---------------------------------------------------------------------*/
;*    rm-unused-vars! ...                                              */
;*    -------------------------------------------------------------    */
;*    Variables with use-count 0 are not used, and can be removed.     */
;*    Attention: external variables are excempted from this rule.      */
;*---------------------------------------------------------------------*/
(define (rm-unused-vars! tree)
   (verbose " removing unused vars")
   (use-count tree)
   (rm! tree #f))

;*---------------------------------------------------------------------*/
;*    rm! ::Node ...                                                   */
;*---------------------------------------------------------------------*/
(define-nmethod (Node.rm!)
   (default-walk! this))

;*---------------------------------------------------------------------*/
;*    rm! ::Set! ...                                                   */
;*---------------------------------------------------------------------*/
(define-nmethod (Set!.rm!)
   (with-access::Set! this (lvalue val)
      (with-access::Ref lvalue (var)
	 (if (isa? var Rm-Var)
	     (walk! val)
	     (default-walk! this)))))

;*---------------------------------------------------------------------*/
;*    rm! ::Let ...                                                    */
;*---------------------------------------------------------------------*/
(define-nmethod (Let.rm!)
   (with-access::Let this (scope-vars)
      (set! scope-vars
	 (filter! (lambda (var)
		     (with-access::Var var (uses)
			(if (zero? uses)
			    (begin
			       (widen!::Rm-Var var)
			       #f)
			    #t)))
	    scope-vars))
      (default-walk! this)))
