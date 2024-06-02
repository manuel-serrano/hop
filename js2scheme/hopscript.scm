;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/hopscript.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu May  6 07:37:36 2021                          */
;*    Last change :  Sun Jun  2 09:34:56 2024 (serrano)                */
;*    Copyright   :  2021-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    A list of functions that traverse the AST after the parsing to   */
;*    make extra hopscript verifications or modifications.             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_hopscript
   
   (include "ast.sch"
	    "usage.sch")

   (import __js2scheme_lexer
	   __js2scheme_html
	   __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils)

   (export (hopscript-mode-fun! ::J2SNode ::symbol)
	   (hopscript-cnst-fun! ::J2SNode ::symbol)
	   (hopscript-let! ::J2SNode ::symbol)
	   (hopscript-async-import! ::J2SNode ::symbol)))

;*---------------------------------------------------------------------*/
;*    hopscript-mode-fun! ...                                          */
;*    -------------------------------------------------------------    */
;*    Propagate the JavaScript mode into the funtion definitions       */ 
;*---------------------------------------------------------------------*/
(define (hopscript-mode-fun! this::J2SNode mode::symbol)
   (mode-fun! this mode))

;*---------------------------------------------------------------------*/
;*    mode-fun! ...                                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (mode-fun! this::J2SNode mode::symbol)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    mode-fun! ::J2SFun ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (mode-fun! this::J2SFun mode)
   (with-access::J2SFun this ((fmode mode) body name)
      (cond
	 ((eq? mode 'hopscript) (set! fmode mode))
	 ((eq? fmode 'normal) (set! fmode mode)))
      (hopscript-mode-fun! body fmode)
      this))

;*---------------------------------------------------------------------*/
;*    mode-fun! ::J2SDeclFun ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (mode-fun! this::J2SDeclFun mode)
   (with-access::J2SDeclFun this (val writable)
      (if writable
	  (call-default-walker)
	  (begin
	     (hopscript-mode-fun! val mode)
	     this))))

;*---------------------------------------------------------------------*/
;*    mode-fun! ::J2SSvc ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (mode-fun! this::J2SSvc mode)
   
   (define (empty-body? body)
      (cond
	 ((isa? body J2SNop)
	  #t)
	 ((isa? body J2SBlock)
	  (with-access::J2SBlock body (nodes)
	     (cond
		((null? nodes) #t)
		((pair? (cdr nodes)) #f)
		(else (empty-body? (car nodes))))))
	 (else
	  #f)))
   
   (with-access::J2SSvc this (loc path body)
      (if (or (eq? mode 'hopscript) (empty-body? body))
	  this
	  (raise
	     (instantiate::&io-parse-error
		(proc "hopc")
		(msg "services are available only in \"hopscript\" mode")
		(obj path)
		(fname (cadr loc))
		(location (caddr loc)))))))

;*---------------------------------------------------------------------*/
;*    hopscript-cnst-fun! ...                                          */
;*    -------------------------------------------------------------    */
;*    In HopScript mode, function declarations are read-only. To       */
;*    implement this, this walker switches J2SDeclFun ronly attribute  */
;*    to #t.                                                           */
;*---------------------------------------------------------------------*/
(define (hopscript-cnst-fun! this::J2SNode mode::symbol)
   (cnst-fun! this))

;*---------------------------------------------------------------------*/
;*    cnst-fun! ...                                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (cnst-fun! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    cnst-fun! ::J2SDeclFun ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (cnst-fun! this::J2SDeclFun)
   (with-access::J2SDeclFun this (val mode writable)
      (with-access::J2SFun val (mode)
	 (when (eq? mode 'hopscript)
	    (set! writable #f)
	    (decl-usage-rem! this 'assig)))
      (if writable
	  (call-default-walker)
	  this)))

;*---------------------------------------------------------------------*/
;*    cnst-fun! ::J2SFun ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (cnst-fun! this::J2SFun)
   (with-access::J2SFun this (decl)
      (when (isa? decl J2SDeclFun)
	 (with-access::J2SDeclFun decl (writable)
	    (unless writable
	       (cnst-fun! decl)))))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    hopscript-let! ...                                               */
;*    -------------------------------------------------------------    */
;*    In HopScript mode, function only supports let declarations and   */
;*    let bindings implicitly introduce new block.                     */
;*---------------------------------------------------------------------*/
(define (hopscript-let! this::J2SNode mode)
   (let! this #t mode))

;*---------------------------------------------------------------------*/
;*    let*! ...                                                        */
;*---------------------------------------------------------------------*/
(define (let*!::pair-nil nodes head::bool mode)

   (define (collect-bindings nodes)
      (let loop ((nodes nodes)
		 (vdecls '())
		 (fdecls '()))
	 (cond
	    ((null? nodes)
	     (values (reverse! vdecls) (reverse! fdecls) nodes))
	    ((isa? (car nodes) J2SVarDecls)
	     (with-access::J2SVarDecls (car nodes) (decls)
		(loop (cdr nodes) (append (reverse decls) vdecls) fdecls)))
	    ((isa? (car nodes) J2SDeclFun)
	     (loop (cdr nodes) vdecls (cons (car nodes) fdecls)))
	    (else
	     (values (reverse! vdecls) (reverse! fdecls) nodes)))))

   (define (letrecblock::J2SLetBlock decls::pair nodes::pair-nil)
      (with-access::J2SNode (car decls) (loc (endloc loc))
	 (for-each (lambda (d)
		      (with-access::J2SDecl d (scope)
			 (set! scope 'letblock)))
	    decls)
	 (J2SLetRecBlock* #t
	    (map (lambda (b) (let! b #f mode)) decls)
	    (if (null? nodes)
		(list (J2SStmtExpr (J2SUndefined)))
		(let*! nodes #f mode)))))
      
   (let loop ((nodes nodes)
	      (head head)
	      (vdecls '())
	      (fdecls '()))
      (cond
	 ((null? nodes)
	  '())
	 ((and (not head)
	       (or (isa? (car nodes) J2SVarDecls)
		   (isa? (car nodes) J2SDeclFun)))
	  (multiple-value-bind (vdecls fdecls nodes)
	     (collect-bindings nodes)
	     (list (letrecblock (append fdecls vdecls) nodes))))
	 ((isa? (car nodes) J2SVarDecls)
	  (with-access::J2SVarDecls (car nodes) (decls)
	     (loop (cdr nodes) head (append (reverse decls) vdecls) fdecls)))
	 ((isa? (car nodes) J2SDeclFun)
	  (loop (cdr nodes) head vdecls (cons (car nodes) fdecls)))
	 (head
	  (append (map! (lambda (d) (let! d #f mode))
		     (append (reverse! vdecls) (reverse! fdecls)))
	     (let*! nodes #f mode)))
	 ((or (pair? vdecls) (pair? fdecls))
	  (let ((decls (append (reverse! fdecls) (reverse! vdecls))))
	     (list (letrecblock decls nodes))))
	 (else
	  (cons (let! (car nodes) #f mode)
	     (loop (cdr nodes) #f '() '()))))))

;*---------------------------------------------------------------------*/
;*    let! ::J2SNode ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (let! this::J2SNode head::bool mode::symbol)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    let! ::J2STilde ...                                              */
;*---------------------------------------------------------------------*/
(define-method (let! this::J2STilde head mode)
   this)

;*---------------------------------------------------------------------*/
;*    let! ::J2SFun ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (let! this::J2SFun head mode)
   (with-access::J2SFun this (mode body)
      (set! body (let! body #t mode))
      this))

;*---------------------------------------------------------------------*/
;*    let! ::J2SDecl ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (let! this::J2SDecl head mode)
   (with-access::J2SDecl this (binder id loc scope)
      (if (and (eq? mode 'hopscript) (eq? binder 'var))
	  (raise
	     (instantiate::&io-parse-error
		(proc "hopc")
		(msg "var keyword not supported in \"hopscript\" mode")
		(obj id)
		(fname (cadr loc))
		(location (caddr loc))))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    let! ::J2SDeclFun ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (let! this::J2SDeclFun head mode)
   (set! head #t)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    let! ::J2SSeq ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (let! this::J2SBlock head mode)
   (with-access::J2SSeq this (nodes)
      (if (eq? mode 'hopscript)
	  (begin
	     (set! nodes (let*! nodes head mode))
	     this)
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    let! ::J2SBlock ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (let! this::J2SBlock head mode)
   (with-access::J2SBlock this (nodes)
      (if (eq? mode 'hopscript)
	  (begin
	     (set! nodes (let*! nodes #f mode))
	     (let unfold ((this this))
		(with-access::J2SBlock this (nodes)
		   (if (and (pair? nodes) (null? (cdr nodes))
			    (isa? (car nodes) J2SBlock))
		       (unfold (car nodes))
		       this))))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    let! ::J2SLetBlock ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (let! this::J2SLetBlock head mode)
   (with-access::J2SBlock this (nodes)
      (if (eq? mode 'hopscript)
	  (begin
	     (set! nodes (let*! nodes #t mode))
	     this)
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    hopscript-async-import! ...                                      */
;*---------------------------------------------------------------------*/
(define (hopscript-async-import! this::J2SNode mode)
   (async-import! this)
   this)

;*---------------------------------------------------------------------*/
;*    async-import! ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (async-import! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    async-import! ::J2SFun ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (async-import! this::J2SFun)
   this)

;*---------------------------------------------------------------------*/
;*    async-import! ::J2SYield ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (async-import! this::J2SYield)
   (with-access::J2SYield this (expr loc)
      (if (isa? expr J2SImportDynamic)
	  (duplicate::J2SImportDynamic expr
	     (promise #f))
	  this)))
