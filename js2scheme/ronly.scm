;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/ronly.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 07:55:23 2013                          */
;*    Last change :  Mon Dec 16 17:55:25 2019 (serrano)                */
;*    Copyright   :  2013-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Mark read-only variables in the J2S AST.                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_ronly

   (include "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils)

   (export j2s-ronly-stage))

;*---------------------------------------------------------------------*/
;*    j2s-ronly-stage ...                                              */
;*---------------------------------------------------------------------*/
(define j2s-ronly-stage
   (instantiate::J2SStageProc
      (name "ronly")
      (comment "Mark read-only variables")
      (proc j2s-ronly)))

;*---------------------------------------------------------------------*/
;*    j2s-ronly ...                                                    */
;*---------------------------------------------------------------------*/
(define (j2s-ronly this args)
   (if (isa? this J2SProgram)
       (j2s-ronly-program this args)
       this))

;*---------------------------------------------------------------------*/
;*    j2s-ronly-program ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-ronly-program this::J2SProgram args)
   (with-access::J2SProgram this (nodes headers decls mode)
      (let ((deval (direct-eval? this)))
	 (when (and (not deval) (not (config-get args :eval)))
	    (init-decls-ronly! (filter (lambda (d) (isa? d J2SDecl)) headers))
	    (init-decls-ronly! decls))
	 (for-each (lambda (o) (ronly! o mode deval)) headers)
	 (for-each (lambda (o) (ronly! o mode deval)) decls)
	 (for-each (lambda (o) (ronly! o mode deval)) nodes))
      this))

;*---------------------------------------------------------------------*/
;*    init-decls-ronly! ...                                            */
;*---------------------------------------------------------------------*/
(define (init-decls-ronly! decls)
   (for-each (lambda (d::J2SDecl)
		(decl-usage-rem! d 'assig))
      decls))

;*---------------------------------------------------------------------*/
;*    direct-eval? ...                                                 */
;*---------------------------------------------------------------------*/
(define (direct-eval? this::J2SProgram)
   (pair? (direct-eval* this)))

;*---------------------------------------------------------------------*/
;*    direct-eval* ...                                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (direct-eval* this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    direct-eval* ::J2SCall ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (direct-eval* this::J2SCall)
   (with-access::J2SCall this (fun)
      (if (isa? fun J2SUnresolvedRef)
	  (with-access::J2SUnresolvedRef fun (id)
	     (if (eq? id 'eval)
		 (list this)
		 '()))
	  '())))

;*---------------------------------------------------------------------*/
;*    ronly! ::J2SNode ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (ronly! this::J2SNode mode::symbol deval::bool)
   (default-walk! this mode deval))

;*---------------------------------------------------------------------*/
;*    ronly! ::J2SAssig ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (ronly! this::J2SAssig mode::symbol deval::bool)

   (define (assig decl loc)
      (when (j2s-const? decl)
	 (with-access::J2SDecl decl (id)
	    (raise
	       (instantiate::&io-error
		  (proc "ronly")
		  (msg "Assignment to constant variable")
		  (obj id)
		  (fname (cadr loc))
		  (location (caddr loc))))))
      (with-access::J2SDecl decl (id)
	 (decl-usage-add! decl 'assig)))
   
   (with-access::J2SAssig this (lhs rhs loc)
      (cond
	 ((isa? lhs J2SRef)
	  (with-access::J2SRef lhs (decl)
	     (assig decl loc)))
	 ((isa? lhs J2SGlobalRef)
	  (with-access::J2SGlobalRef lhs (decl)
	     (assig decl loc))))
      (ronly! rhs mode deval))
   this)

;*---------------------------------------------------------------------*/
;*    ronly! ::J2SInit ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (ronly! this::J2SInit mode::symbol deval::bool)
   (with-access::J2SAssig this (lhs rhs)
      (when (isa? lhs J2SRef)
	 (with-access::J2SRef lhs (decl)
	    (with-access::J2SDecl decl ( id)
	       (decl-usage-add! decl 'assig))))
      (ronly! rhs mode deval))
   this)

;*---------------------------------------------------------------------*/
;*    ronly! ::J2SUnary ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (ronly! this::J2SUnary mode::symbol deval::bool)
   (with-access::J2SUnary this (op expr)
      (if (and (eq? op 'delete) (isa? expr J2SRef))
	  (with-access::J2SRef expr (decl)
	     (with-access::J2SDecl decl (id)
		(decl-usage-add! decl 'delete))
	     this)
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    ronly! ::J2SDeclInit ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (ronly! this::J2SDeclInit mode::symbol deval::bool)
   (with-access::J2SDeclInit this (id key val)
      (ronly! val mode deval))
   this)

;*---------------------------------------------------------------------*/
;*    ronly! ::J2SBlock ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (ronly! this::J2SBlock mode::symbol deval::bool)
   (with-access::J2SBlock this (nodes)
      (for-each (lambda (n::J2SNode)
		   (when (isa? n J2SDecl)
		      (decl-usage-rem! n 'assig)))
	 nodes)
      (call-default-walker)))
      
;*---------------------------------------------------------------------*/
;*    ronly! ::J2SLetBlock ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (ronly! this::J2SLetBlock mode::symbol deval::bool)
   (with-access::J2SLetBlock this (decls)
      (for-each (lambda (d::J2SDecl) (decl-usage-rem! d 'assig)) decls)
      (call-next-method)))
		   
;*---------------------------------------------------------------------*/
;*    ronly! ::J2SVarDecls ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (ronly! this::J2SVarDecls mode::symbol deval::bool)
   (with-access::J2SVarDecls this (decls)
      (for-each (lambda (d::J2SDecl) (decl-usage-rem! d 'assig)) decls)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    ronly! ::J2SFun ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (ronly! this::J2SFun mode::symbol deval::bool)
   (with-access::J2SFun this (params thisp mode body)
      (when thisp
	 (decl-usage-rem! thisp 'assig))
      (for-each (lambda (d::J2SDecl) (decl-usage-rem! d 'assig)) params)
      (set! body (ronly! body mode deval))
      this))

;*---------------------------------------------------------------------*/
;*    ronly! ::J2SWith ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (ronly! this::J2SWith mode::symbol deval::bool)
   (with-access::J2SWith this (block)
      (mark-write! block)
      this))

;*---------------------------------------------------------------------*/
;*    mark-write! ::J2SNode ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-write! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    mark-write! ::J2SRef ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-write! this::J2SRef)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (id)
	 (decl-usage-add! decl 'assig)
	 this)))
   
