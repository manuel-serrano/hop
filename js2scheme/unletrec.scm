;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/unletrec.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May 15 09:53:30 2018                          */
;*    Last change :  Tue May 29 07:13:04 2018 (serrano)                */
;*    Copyright   :  2018 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Letrec optimization                                              */
;*    -------------------------------------------------------------    */
;*    This optimization transforms letrec into let(*). This improves   */
;*    generated code because it avoids boxing mutated rec variables    */
;*    and it enables other optimization such as PCE.                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_unletrec

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils
	   __js2scheme_alpha
	   __js2scheme_use)

   (static (class UDeclInfo
	      (optdecl (default #unspecified))
	      (used (default #unspecified))
	      (init (default #unspecified)))
	   
	   (class UFunInfo
	      (used::pair-nil (default '()))
	      (idecls::pair-nil (default '()))))

   (export j2s-unletrec-stage))

;*---------------------------------------------------------------------*/
;*    j2s-unletrec-stage ...                                           */
;*---------------------------------------------------------------------*/
(define j2s-unletrec-stage
   (instantiate::J2SStageProc
      (name "unletrec")
      (comment "Letrec splitting optimization")
      (proc j2s-unletrec!)
      (optional :optim-unletrec)))

;*---------------------------------------------------------------------*/
;*    j2s-unletrec! ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-unletrec! this args)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes headers decls)
	 (for-each (lambda (o) (unletrec! o)) headers)
	 (for-each (lambda (o) (unletrec! o)) decls)
	 (for-each (lambda (o) (unletrec! o)) nodes)))
   this)

;*---------------------------------------------------------------------*/
;*    unletrec! ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (unletrec! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    unletrec! ::J2SLetBlock ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (unletrec! this::J2SLetBlock)
   (with-access::J2SLetBlock this (rec decls nodes loc)
      (if (not rec)
	  (call-default-walker)
	  (let loop ((ds decls)
		     (d* '())
		     (drec '()))
	     (cond
		((null? ds)
		 (cond
		    ((pair? d*)
		     (duplicate::J2SLetBlock this
			(rec #f)
			(decls (reverse! d*))
			(nodes (map! unletrec! nodes))))
		    ((pair? drec)
		     (duplicate::J2SLetBlock this
			(decls (reverse! drec))
			(nodes (map! unletrec! nodes))))
		    (else
		     (J2SSeq (map! unletrec! nodes)))))
		((or (not (isa? (car ds) J2SDeclInit))
		     (null? (get-used-decls (car ds) ds)))
		 (if (pair? drec)
		     (let ((used (append-map (lambda (d)
						(get-used-decls d ds))
				    drec)))
			(if (null? used)
			    (duplicate::J2SLetBlock this
			       (rec #t)
			       (decls (reverse! drec))
			       (nodes (list (loop (cdr ds) (list (car ds)) '()))))
			    (loop (cdr ds) '() (cons (car ds) drec))))
		     (loop (cdr ds) (cons (car ds) d*) '())))
		((pair? d*)
		 (duplicate::J2SLetBlock this
		    (rec #f)
		    (decls (reverse! d*))
		    (nodes (list (loop (cdr ds) '() (cons (car ds) drec))))))
		((pair? drec)
		 (let ((used (append-map (lambda (d)
					    (get-used-decls d ds))
				drec)))
		    (if (null? used)
			(duplicate::J2SLetBlock this
			   (rec #t)
			   (decls (reverse! drec))
			   (nodes (list (loop (cdr ds) '() (list (car ds))))))
			(loop (cdr ds) '() (cons (car ds) drec)))))
		(else
		 (loop (cdr ds) '() (cons (car ds) drec))))))))
		
;*---------------------------------------------------------------------*/
;*    get-used-decls ...                                               */
;*    -------------------------------------------------------------    */
;*    Amongst DECLS, returns those that appear in NODE.                */
;*---------------------------------------------------------------------*/
(define (get-used-decls node::J2SNode decls::pair-nil)
   (delete-duplicates! (node-used* node decls #t)))

;*---------------------------------------------------------------------*/
;*    node-used* ...                                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (node-used* node::J2SNode decls store)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    node-used* ::J2SDecl ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-used* node::J2SDecl decls store)
   (if (member node decls) (list node) '()))

;*---------------------------------------------------------------------*/
;*    node-used* ::J2Ref ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (node-used* node::J2SRef decls store)
   (with-access::J2SRef node (decl)
      (if (member decl decls) (list decl) '())))

;*---------------------------------------------------------------------*/
;*    node-used* ::J2SDeclInit ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (node-used* node::J2SDeclInit decls store)
   (with-access::J2SDeclInit node (val)
      (node-used* val decls store)))
      
;*---------------------------------------------------------------------*/
;*    node-used* ::J2SFun ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-used* node::J2SFun decls store)
   (with-access::J2SFun node (%info body decl)
      (if (and (isa? %info UFunInfo)
	       (with-access::UFunInfo %info (idecls)
		  (equal? idecls decls)))
	  (with-access::UFunInfo %info (used decls)
	     used)
	  (let ((info (instantiate::UFunInfo (idecls decls))))
	     (set! %info info)
	     (let ((bodyused (node-used* body decls #f)))
		(if store
		    (begin
		       (with-access::UFunInfo info (used)
			  (set! used bodyused))
		       (when (isa? decl J2SDecl)
			  (with-access::J2SDecl decl (%info)
			     (if (isa? %info UDeclInfo)
				 (with-access::UDeclInfo %info (used)
				    (set! used bodyused))
				 (set! %info
				    (instantiate::UDeclInfo
				       (used bodyused)))))))
		    (set! %info #f))
		bodyused)))))
