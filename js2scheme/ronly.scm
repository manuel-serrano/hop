;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/ronly.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 07:55:23 2013                          */
;*    Last change :  Sat Dec  9 06:19:49 2017 (serrano)                */
;*    Copyright   :  2013-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Mark read-only variables in the J2S AST.                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_ronly
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage)

   (export j2s-ronly-stage))

;*---------------------------------------------------------------------*/
;*    j2s-ronly-stage ...                                              */
;*---------------------------------------------------------------------*/
(define j2s-ronly-stage
   (instantiate::J2SStageProc
      (name "read-only")
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
      (let ((mode (cond
		     ((eq? mode 'hopscript) mode)
		     ((or (not (eq? mode 'strict)) (direct-eval? this)) mode)
		     (else 'hopscript))))
	 (when (eq? mode 'hopscript)
	    (init-decls-ronly! decls))
	 (for-each (lambda (o) (ronly! o mode)) headers)
	 (for-each (lambda (o) (ronly! o mode)) decls)
	 (for-each (lambda (o) (ronly! o mode)) nodes))
      this))

;*---------------------------------------------------------------------*/
;*    init-decls-ronly! ...                                            */
;*---------------------------------------------------------------------*/
(define (init-decls-ronly! decls)
   (for-each (lambda (d::J2SDecl)
		(with-access::J2SDecl d (ronly)
		   (set! ronly #t)))
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
(define-walk-method (ronly! this::J2SNode mode::symbol)
   (default-walk! this mode))

;*---------------------------------------------------------------------*/
;*    ronly! ::J2SAssig ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (ronly! this::J2SAssig mode::symbol)

   (define (assig decl loc)
      (when (j2s-const? decl)
	 (with-access::J2SDecl decl (id)
	    (raise
	       (instantiate::&io-error
		  (proc "ronly")
		  (msg "Const variables cannot be assigned")
		  (obj id)
		  (fname (cadr loc))
		  (location (caddr loc))))))
      (with-access::J2SDecl decl (ronly)
	 (set! ronly #f)))
   
   (with-access::J2SAssig this (lhs rhs loc)
      (cond
	 ((isa? lhs J2SRef)
	  (with-access::J2SRef lhs (decl)
	     (assig decl loc)))
	 ((isa? lhs J2SGlobalRef)
	  (with-access::J2SGlobalRef lhs (decl)
	     (assig decl loc))))
      (ronly! rhs mode))
   this)

;*---------------------------------------------------------------------*/
;*    ronly! ::J2SInit ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (ronly! this::J2SInit mode::symbol)
   (with-access::J2SAssig this (lhs rhs)
      (when (isa? lhs J2SRef)
	 (with-access::J2SRef lhs (decl)
	    (with-access::J2SDecl decl (ronly id)
	       (set! ronly #f))))
      (ronly! rhs mode))
   this)

;*---------------------------------------------------------------------*/
;*    ronly! ::J2SUnary ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (ronly! this::J2SUnary mode::symbol)
   (with-access::J2SUnary this (op expr)
      (if (and (eq? op 'delete) (isa? expr J2SRef))
	  (with-access::J2SRef expr (decl)
	     (with-access::J2SDecl decl (ronly)
		(set! ronly #f))
	     this)
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    ronly-decl! ...                                                  */
;*---------------------------------------------------------------------*/
(define (ronly-decl! this::J2SDecl mode::symbol)
   (with-access::J2SDecl this (ronly scope writable)
      (if (eq? mode 'hopscript)
	  (set! ronly #t)
	  (set! ronly (or (not (memq scope '(global %scope))) (not writable)))))
   this)

;*---------------------------------------------------------------------*/
;*    ronly! ::J2SDeclInit ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (ronly! this::J2SDeclInit mode::symbol)
   (call-next-method)
   (with-access::J2SDeclInit this (val ronly)
      (ronly! val mode))
   this)

;*---------------------------------------------------------------------*/
;*    ronly! ::J2SLetBlock ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (ronly! this::J2SLetBlock mode::symbol)
   (with-access::J2SLetBlock this (decls)
      (for-each (lambda (d::J2SDecl) (ronly-decl! d mode)) decls)
      (call-next-method)))
		   
;*---------------------------------------------------------------------*/
;*    ronly! ::J2SVarDecls ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (ronly! this::J2SVarDecls mode::symbol)
   (with-access::J2SVarDecls this (decls)
      (for-each (lambda (d::J2SDecl) (ronly-decl! d mode)) decls)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    ronly! ::J2SFun ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (ronly! this::J2SFun mode::symbol)
   (with-access::J2SFun this (params thisp)
      (when thisp (with-access::J2SDecl thisp (ronly) (set! ronly #t)))
      (for-each (lambda (d::J2SDecl) (ronly-decl! d mode)) params)
      (call-next-method)))

;* {*---------------------------------------------------------------------*} */
;* {*    ronly! ::J2SLetInit ...                                          *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (ronly! this::J2SLetInit)                       */
;*    (call-next-method)                                               */
;*    (with-access::J2SLetInit this (val)                              */
;*       (ronly! val))                                                 */
;*    this)                                                            */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    ronly! ::J2SLetOpt ...                                           *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (ronly! this::J2SLetOpt)                        */
;*    (call-next-method)                                               */
;*    (with-access::J2SLetOpt this (val id)                            */
;*       (ronly! val))                                                 */
;*    this)                                                            */
