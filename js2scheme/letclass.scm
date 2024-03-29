;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/letclass.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jun 28 06:35:14 2015                          */
;*    Last change :  Sat Nov 13 10:14:02 2021 (serrano)                */
;*    Copyright   :  2015-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Let class optimization. This optimization implements a single    */
;*    transformation: immutable declclasses bound to undefined are     */
;*    directly bound to classes but the classes are marked as neeeding */
;*    dead-zone checks.                                                */
;*    -------------------------------------------------------------    */
;*    The optimization applies if the class uses variables that are    */
;*    all bound at the declaration site. This is computed using a      */
;*    a simple approximation, classes are optimized if initialized     */
;*    in the block that declares them.                                 */
;*    -------------------------------------------------------------    */
;*    This optimization cannot run before the "use" stage.             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_letclass

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_lexer
	   __js2scheme_alpha
	   __js2scheme_usage
	   __js2scheme_freevars)

   (export j2s-letclass-stage))

;*---------------------------------------------------------------------*/
;*    j2s-letclass-stage ...                                           */
;*---------------------------------------------------------------------*/
(define j2s-letclass-stage
   (instantiate::J2SStageProc
      (name "letclass")
      (comment "Class declarations optimization")
      (proc j2s-letclass)
      (optional :optim-letclass)))

;*---------------------------------------------------------------------*/
;*    j2s-letclass ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-letclass this args)
   (when (isa? this J2SProgram)
      (mark-declclass this #f args)
      (letclass! this #f args)
   this))

;*---------------------------------------------------------------------*/
;*    mark-declclass ::J2SNode ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-declclass this::J2SNode lblock args)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    mark-declclass ::J2SLetBlock ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-declclass this::J2SLetBlock lblock args)
   (with-access::J2SLetBlock this (decls nodes)
      (for-each (lambda (d) (mark-declclass d lblock args)) decls)
      (for-each (lambda (n) (mark-declclass n this args)) nodes)))

;*---------------------------------------------------------------------*/
;*    mark-declclass ::J2SDeclClass ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-declclass this::J2SDeclClass lblock args)
   (call-default-walker)
   (with-access::J2SDeclClass this (%info)
      (set! %info (cons 'lblock lblock))))

;*---------------------------------------------------------------------*/
;*    letclass! ...                                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (letclass! this::J2SNode lblock args)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    letclass! ::J2SLetBlock ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (letclass! this::J2SLetBlock lblock args)
   (with-access::J2SLetBlock this (decls nodes)
      (set! decls (map (lambda (d) (letclass! d lblock args)) decls))
      (set! nodes (map (lambda (n) (letclass! n this args)) nodes))
      this))

;*---------------------------------------------------------------------*/
;*    letclass! ::J2SInit ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (letclass! this::J2SInit lblock args)
   (with-access::J2SInit this (lhs rhs loc)
      (call-default-walker)
      (if (and (isa? lhs J2SRef) (isa? rhs J2SClass))
	  (with-access::J2SRef lhs (decl)
	     (if (isa? decl J2SDeclClass)
		 (if (not (decl-usage-has? decl '(assig)))
		     (with-access::J2SDeclClass decl (val binder %info)
			(if (and (isa? val J2SUndefined)
				 (pair? %info)
				 (isa? (cdr %info) J2SLetBlock)
				 (eq? (cdr %info) lblock ))
			    (with-access::J2SClass rhs (need-dead-zone-check)
			       (set! binder 'let)
			       (decl-usage-add! decl 'uninit)
			       (set! need-dead-zone-check #t)
			       (set! val rhs)
			       (J2SUndefined))
			    this))
		     this)
		 this))
	  this)))


	  
