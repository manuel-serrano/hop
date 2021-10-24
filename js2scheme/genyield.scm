;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/genyield.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 11 14:30:38 2013                          */
;*    Last change :  Sun Oct 24 10:19:57 2021 (serrano)                */
;*    Copyright   :  2013-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Yield heap->stack allocation                                     */
;*    -------------------------------------------------------------    */
;*    This optimization attempts to allocate yield values onto the     */
;*    stack. For that it detects the following patterns:               */
;*      let x = expr.next()                                            */
;*      x.done                                                         */
;*      x.value                                                        */
;*    When such a pattern is found the call expr.next is replaced      */
;*    with maybeGeneratorNext(expr,a fresh-stack-yield-value).         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_genyield

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils
	   __js2scheme_usage)

   (export j2s-genyield-stage
	   (generic j2s-genyield ::obj ::obj)))

;*---------------------------------------------------------------------*/
;*    j2s-genyield-stage ...                                           */
;*---------------------------------------------------------------------*/
(define j2s-genyield-stage
   (instantiate::J2SStageProc
      (name "genyield")
      (comment "Detect next().{done,value} to stack allocate yield values")
      (proc j2s-genyield)
      (optional :optim-stack-alloc)))

;*---------------------------------------------------------------------*/
;*    yieldinfo ...                                                    */
;*---------------------------------------------------------------------*/
(define-struct yieldinfo valid cntvalue cntdone)

;*---------------------------------------------------------------------*/
;*    j2s-genyield ...                                                 */
;*---------------------------------------------------------------------*/
(define-generic (j2s-genyield this args)
   this)

;*---------------------------------------------------------------------*/
;*    j2s-genyield ::J2SProgram ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s-genyield this::J2SProgram args)
   (with-access::J2SProgram this (headers decls nodes)
      (for-each (lambda (o) (genyield! o args)) headers)
      (for-each (lambda (o) (genyield! o args)) decls)
      (for-each (lambda (o) (genyield! o args)) nodes))
   this)

;*---------------------------------------------------------------------*/
;*    yield-field-access ...                                           */
;*---------------------------------------------------------------------*/
(define (yield-field-access expr::J2SExpr)
   (when (isa? expr J2SAccess)
      (with-access::J2SAccess expr (obj field)
	 (when (and (isa? obj J2SRef) (isa? field J2SString))
	    (with-access::J2SRef obj (decl)
	       (with-access::J2SDecl decl (%info)
		  (when (and (yieldinfo? %info) (yieldinfo-valid %info))
		     (with-access::J2SString field (val)
			val))))))))

;*---------------------------------------------------------------------*/
;*    genyield! ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (genyield! this::J2SNode args)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    genyield! ::J2SLetBlock ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (genyield! this::J2SLetBlock args)
   
   (define (maybe-next-call? expr::J2SExpr)
      (when (isa? expr J2SCall)
	 (with-access::J2SCall expr (fun args)
	    (when (<=fx (length args) 2)
	       (when (isa? fun J2SAccess)
		  (with-access::J2SAccess fun (obj field)
		     (when (isa? field J2SString)
			(with-access::J2SString field (val)
			   (string=? val "next")))))))))

   (define (stackable? decl)
      (when (isa? decl J2SDeclInit)
	 (with-access::J2SDeclInit decl (%info usecnt)
	    (when (yieldinfo? %info)
	       (let ((safeuse (+fx (yieldinfo-cntdone %info) (yieldinfo-cntvalue %info))))
		  (=fx usecnt safeuse))))))

   (define (yield-stack-alloc this::J2SDecl)
      (with-access::J2SDecl this (loc id)
	 (J2SLetOptVUtype 'object '(ref &ref) (gensym id)
	    (J2SPragma '(js-make-stack-yield)))))

   (define (maybe-next-call this::J2SCall decl)
      (with-access::J2SCall this (fun args loc protocol)
	 (if (eq? protocol 'spread)
	     (with-access::J2SAccess fun (obj)
		(with-access::J2SSpread (car args) (expr)
		   (J2SHopCall
		      (J2SHopRef 'js-generator-maybe-next-spread)
		      obj (J2SRef decl) expr (J2SHopRef '%this))))
	     (with-access::J2SAccess fun (obj)
		(J2SHopCall*
		   (J2SHopRef
		      (string->symbol
			 (format "js-generator-maybe-next~a"
			    (length args))))
		   (cons* obj (J2SRef decl)
		      (append args (list (J2SHopRef '%this)))))))))
   
   (with-access::J2SLetBlock this (decls)
      (let ((ds (filter (lambda (decl)
			   (when (isa? decl J2SDeclInit)
			      (with-access::J2SDeclInit decl (val escape %info id loc)
				 (when (and (not escape) (maybe-next-call? val))
				    (set! %info (yieldinfo #t 0 0))))))
		   decls)))
	 (call-default-walker)
	 (for-each (lambda (decl)
		      (when (stackable? decl)
			 (with-access::J2SDeclInit decl (val)
			    (let ((ndecl (yield-stack-alloc decl)))
			       (set! decls (cons ndecl decls))
			       (set! val (maybe-next-call val ndecl))))))
	    decls))
      this))

;*---------------------------------------------------------------------*/
;*    genyield! ::J2SRef ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (genyield! this::J2SRef args)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (%info)
	 (when (yieldinfo? %info)
	    (yieldinfo-valid-set! %info #f))))
   this)

;*---------------------------------------------------------------------*/
;*    genyield! ::J2SAccess ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (genyield! this::J2SAccess args)
   (let ((field (yield-field-access this)))
      (cond
	 ((not field)
	  (call-default-walker))
	 ((string=? field "value")
	  (with-access::J2SAccess this (obj)
	     (with-access::J2SRef obj (decl)
		(with-access::J2SDecl decl (%info)
		   (yieldinfo-cntvalue-set! %info
		      (+fx 1 (yieldinfo-cntvalue %info))))))
	  this)
	 ((string=? field "done")
	  (with-access::J2SAccess this (obj)
	     (with-access::J2SRef obj (decl)
		(with-access::J2SDecl decl (%info)
		   (yieldinfo-cntdone-set! %info
		      (+fx 1 (yieldinfo-cntdone %info))))))
	  this)
	 (else
	  (call-default-walker)))))

;*---------------------------------------------------------------------*/
;*    genyield! ::J2SAssig ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (genyield! this::J2SAssig args)
   (with-access::J2SAssig this (lhs rhs)
      (set! rhs (genyield! rhs args))
      (if (isa? lhs J2SAccess)
	  (with-access::J2SAccess lhs (obj field)
	     (set! obj (genyield! obj args))
	     (set! field (genyield! field args)))
	  (set! lhs (genyield! lhs args)))
      this))
		
   
