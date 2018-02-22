;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/ctor.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb  1 13:36:09 2017                          */
;*    Last change :  Thu Feb 22 09:07:21 2018 (serrano)                */
;*    Copyright   :  2017-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Static approximation of constructors sizes                       */
;*    -------------------------------------------------------------    */
;*    This stage must executes after the property stage as the         */
;*    cache indexes are needed.                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_ctor

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils
	   __js2scheme_type-hint)

   (export j2s-ctor-stage))

;*---------------------------------------------------------------------*/
;*    j2s-ctor-stage ...                                               */
;*---------------------------------------------------------------------*/
(define j2s-ctor-stage
   (instantiate::J2SStageProc
      (name "ctor")
      (comment "Type ctors")
      (proc j2s-ctor!)))

;*---------------------------------------------------------------------*/
;*    j2s-ctor! ::obj ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-ctor! this args)
   (when (isa? this J2SProgram)
      (j2s-ctor-program! this args)
      this))

;*---------------------------------------------------------------------*/
;*    j2s-ctor-program! ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-ctor-program! this::J2SProgram args)
   (with-access::J2SProgram this (decls nodes)
      ;; compute a conservative approximation of ctor sizes
      (for-each (lambda (n) (constrsize! n)) decls)
      (for-each (lambda (n) (constrsize! n)) nodes)
      ;; improve the init sequences this.a = x; this.b = y; ...
      (when (config-get args :optim-ctor #f)
	 (when (>=fx (config-get args :verbose 0) 4)
	    (fprintf (current-error-port) " [optim-ctor]"))
	 (for-each (lambda (n) (constrinit! n this)) decls)
	 (for-each (lambda (n) (constrinit! n this)) nodes))
      this))

;*---------------------------------------------------------------------*/
;*    constrsize! ::J2SNode ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (constrsize! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    constrsize! ::J2SFun ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (constrsize! this::J2SFun)
   (with-access::J2SFun this (body constrsize)
      (let ((acc (make-cell 0)))
	 (count-this-assig body acc)
	 (set! constrsize (cell-ref acc)))
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    count-this-assig ::J2SNode ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (count-this-assig this::J2SNode acc::cell)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    count-this-assig ::J2SAssig ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (count-this-assig this::J2SAssig acc::cell)
   (with-access::J2SAssig this (lhs rhs)
      (call-default-walker)
      (when (isa? lhs J2SAccess)
	 (with-access::J2SAccess lhs (obj)
	    (when (isa? obj J2SThis)
	       (cell-set! acc (+fx 1 (cell-ref acc))))))))

;*---------------------------------------------------------------------*/
;*    count-this-assig ::J2SCond ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (count-this-assig this::J2SCond acc::cell)
   (with-access::J2SCond this (test then else)
      (count-this-assig test acc)
      (let ((athen (make-cell 0))
	    (aelse (make-cell 0)))
	 (count-this-assig then athen)
	 (count-this-assig else aelse)
	 (cell-set! acc
	    (+fx (cell-ref acc) (max (cell-ref athen) (cell-ref aelse)))))))

;*---------------------------------------------------------------------*/
;*    count-this-assig ::J2SIf ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (count-this-assig this::J2SIf acc::cell)
   (with-access::J2SIf this (test then else)
      (count-this-assig test acc)
      (let ((athen (make-cell 0))
	    (aelse (make-cell 0)))
	 (count-this-assig then athen)
	 (count-this-assig else aelse)
	 (cell-set! acc
	    (+fx (cell-ref acc) (max (cell-ref athen) (cell-ref aelse)))))))

;*---------------------------------------------------------------------*/
;*    constrinit! ::J2SNode ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (constrinit! this::J2SNode prog::J2SProgram)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    constrinit! ::J2SDeclInit ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (constrinit! this::J2SDeclInit prog)
   (with-access::J2SDeclInit this (id usage val)
      (when (and (isa? val J2SFun) (or #t (memq 'new usage)))
	 (constrinit-ctor! val prog)))
   this)

;*---------------------------------------------------------------------*/
;*    constrinit! ::J2SDeclFun ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (constrinit! this::J2SDeclFun prog)
   (with-access::J2SDeclFun this (id usage val)
      (when (and (isa? val J2SFun) (or #t (memq 'new usage)))
	 (constrinit-ctor! val prog)))
   this)

;*---------------------------------------------------------------------*/
;*    constrinit-ctor! ...                                             */
;*---------------------------------------------------------------------*/
(define (constrinit-ctor! fun::J2SFun prog::J2SProgram)
   (with-access::J2SFun fun (body)
      (set! body (constrinit-seq! body prog))))

;*---------------------------------------------------------------------*/
;*    constrinit-seq! ::J2SNode ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (constrinit-seq! this::J2SNode prog::J2SProgram)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    constrinit-seq! ::J2SBlock ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (constrinit-seq! this::J2SBlock prog)

   (define (simple-expr? expr obj)
      ;; is the expr simple enough so we are certain that there is no
      ;; occurrent of decl involved
      (cond
	 ((isa? expr J2SArray)
	  (with-access::J2SArray expr (exprs)
	     (every (lambda (e) (simple-expr? e obj)) exprs)))
	 ((isa? expr J2SLiteral)
	  #t)
	 ((isa? expr J2SRef)
	  (with-access::J2SRef expr (decl)
	     (not (eq? decl obj))))
	 ((isa? expr J2SParen)
	  (with-access::J2SParen expr (expr)
	     (simple-expr? expr obj)))
	 ((isa? expr J2SUnary)
	  (with-access::J2SUnary expr (expr)
	     (simple-expr? expr obj)))
	 ((isa? expr J2SBinary)
	  (with-access::J2SBinary expr (lhs rhs)
	     (and (simple-expr? lhs obj) (simple-expr? rhs obj))))
	 ((isa? expr J2SCond)
	  (with-access::J2SCond expr (test then else)
	     (and (simple-expr? test obj)
		  (simple-expr? then obj)
		  (simple-expr? else obj))))
	 (else
	  #f)))
	     
   (define (obj-assign node ref-or-bool)
      ;; check the syntactic form
      ;; (J2SStmtExpr (J2SAssig (J2SAccess (J2SRef) (J2SString ...)) ..)))
      ;; and check that in (J2SRef decl), decl is an object
      (when (isa? node J2SStmtExpr)
	 (with-access::J2SStmtExpr node (expr)
	    (when (isa? expr J2SAssig)
	       (with-access::J2SAssig expr (lhs rhs)
		  (when (isa? lhs J2SAccess)
		     (with-access::J2SAccess lhs (obj field)
			(when (isa? field J2SString)
			   (when (isa? obj J2SRef)
			      (with-access::J2SRef obj (decl)
				 (if ref-or-bool
				     (with-access::J2SRef ref-or-bool ((rd decl))
					(when (eq? rd decl)
					   (simple-expr? rhs decl)))
				     (with-access::J2SDecl decl (itype)
					(when (eq? itype 'object)
					   obj)))))))))))))
   
   (define (split-init-sequence this)
      ;; split a block in two part
      ;;   1- the "ref" assignments
      ;;   2- the other statements
      (with-access::J2SBlock this (nodes)
	 (if (or (null? nodes) (null? (cdr nodes)))
	     (values '() nodes #f)
	     (let ((ref (obj-assign (car nodes) #f)))
		(if (not ref)
		    (values '() nodes #f)
		    (let loop ((ns (cdr nodes))
			       (prev nodes))
		       (cond
			  ((null? ns)
			   (values nodes '() ref))
			  ((obj-assign (car ns) ref)
			   (loop (cdr ns) ns))
			  (else
			   (set-cdr! prev '())
			   (values nodes ns ref)))))))))
   
   (multiple-value-bind (init rest ref)
      (split-init-sequence this)
      (cond
	 ((null? init)
	  ;; no init, just a regular block
	  (call-default-walker))
	 ((<fx (length init) 5)
	  ;; too small to be optimized, not worse the bookkeeping
	  (set-cdr! (last-pair init) rest)
	  (call-default-walker))
	 (else
	  ;; optimize the init sequence, first create two program globals
	  (let ((cmap0 (gensym '%cmap0))
		(cmap1 (gensym '%cmap1))
		(offset (gensym '%offset)))
	     (with-access::J2SProgram prog (globals)
		(set! globals
		   (cons* `(define ,cmap0 #f)
		      `(define ,cmap1 #f)
		      `(define ,offset -1)
		      globals)))
	     ;; then split the init sequence
	     (with-access::J2SBlock this (nodes loc)
		(set! nodes 
		   (cons (instantiate::J2SOPTInitSeq
			    (loc loc)
			    (ref ref)
			    (nodes init)
			    (cmap0 cmap0)
			    (cmap1 cmap1)
			    (offset offset))
		      (map! (lambda (n) (constrinit-seq! n prog)) rest))))
	     this)))))
