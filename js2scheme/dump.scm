;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/dump.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 11 11:12:21 2013                          */
;*    Last change :  Tue May 29 08:24:06 2018 (serrano)                */
;*    Copyright   :  2013-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Dump the AST for debugging                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_dump
   
   (import __js2scheme_ast
	   __js2scheme_utils
	   __js2scheme_stage)

   (export j2s-dump-stage
	   (j2s-dump-decls::obj ::obj)
	   (generic j2s->list ::obj)
	   (generic j2s-info->list ::obj)))

;*---------------------------------------------------------------------*/
;*    j2s-dump-stage ...                                               */
;*---------------------------------------------------------------------*/
(define j2s-dump-stage
   (instantiate::J2SStageProc
      (name "dump")
      (comment "Dump the AST for debug")
      (proc j2s-dump)
      (optional #t)))

;*---------------------------------------------------------------------*/
;*    dump-info ...                                                    */
;*---------------------------------------------------------------------*/
(define (dump-info this::J2SNode)
   (with-access::J2SNode this (%info)
      (if (and (not (eq? %info #unspecified))
	       (not (null? %info))
	       (or (>= (bigloo-debug) 3)
		   (string-contains (or (getenv "HOPTRACE") "") "j2s:info")))
	  `(:%info ,(j2s-info->list %info))
	  '())))

;*---------------------------------------------------------------------*/
;*    j2s-info->list ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (j2s-info->list this)
   (cond
      ((and (pair? this) (null? (cdr this)))
       (cons (j2s-info->list (car this)) (j2s-info->list (cdr this))))
      ((list? this)
       (map j2s-info->list this))
      ((or (string? this) (symbol? this) (number? this) (boolean? this))
       this)
      ((or (char? this) (eq? this #unspecified))
       this)
      ((struct? this)
       (let ((o (make-struct (struct-key this) (struct-length this) #f)))
	  (let loop ((i 0))
	     (if (=fx i (struct-length this))
		 o
		 (begin
		    (struct-set! o i (j2s-info->list (struct-ref this i)))
		    (loop (+fx i 1)))))))
      ((isa? this J2SString)
       (with-access::J2SString this (val)
	  (format "[[J2SString:~a]]" val)))
      ((isa? this J2SDecl)
       (with-access::J2SDecl this (id)
	  (format "[[J2SDecl:~a]]" id)))
      (else
       (format "[[~a]]" (typeof this)))))

;*---------------------------------------------------------------------*/
;*    dump-dump ...                                                    */
;*---------------------------------------------------------------------*/
(define (dump-dump this::J2SNode)
   (with-access::J2SNode this (%%dump)
      (if (or (>= (bigloo-debug) 2)
	      (string-contains (or (getenv "HOPTRACE") "") "j2s:dump"))
	  (if (eq? %%dump #unspecified)
	      '()
	      (list :%%dump %%dump))
	  '())))

;*---------------------------------------------------------------------*/
;*    dump-type ...                                                    */
;*---------------------------------------------------------------------*/
(define (dump-type this::J2SExpr)
   (with-access::J2SExpr this (type)
      (if (or (>= (bigloo-debug) 2)
	      (string-contains (or (getenv "HOPTRACE") "") "j2s:type"))
	  `(:type ,type)
	  '())))

;*---------------------------------------------------------------------*/
;*    dump-rtype ...                                                   */
;*---------------------------------------------------------------------*/
(define (dump-rtype this)
   (with-access::J2SFun this (rtype)
      (if (or (>= (bigloo-debug) 2)
	      (string-contains  (or (getenv "HOPTRACE") "") "j2s:type"))
	  `(:rtype ,rtype)
	  '())))
      
;*---------------------------------------------------------------------*/
;*    dump-vtype ...                                                   */
;*---------------------------------------------------------------------*/
(define (dump-vtype this::J2SDecl)
   (with-access::J2SDecl this (vtype)
      (if (or (>= (bigloo-debug) 2)
	      (string-contains (or (getenv "HOPTRACE") "") "j2s:type"))
	  `(:vtype ,vtype)
	  '())))
      
;*---------------------------------------------------------------------*/
;*    dump-scope ...                                                   */
;*---------------------------------------------------------------------*/
(define (dump-scope scope)
   (if (or (>= (bigloo-debug) 3)
	   (string-contains (or (getenv "HOPTRACE") "") "j2s:scope"))
       `(:scope ,scope)
       '()))
      
;*---------------------------------------------------------------------*/
;*    dump-itype ...                                                   */
;*---------------------------------------------------------------------*/
(define (dump-itype this::J2SDecl)
   (with-access::J2SDecl this (itype utype vtype)
      (cond
	 ((>= (bigloo-debug) 2)
	  `(:utype ,utype :itype ,itype :vtype ,vtype))
	 ((string-contains (or (getenv "HOPTRACE") "") "j2s:utype")
	  `( :utype ,utype :itype ,itype :vtype ,vtype))
	 ((string-contains (or (getenv "HOPTRACE") "") "j2s:type")
	  `(:itype ,itype :vtype ,vtype))
	 (else
	  '()))))
      
;*---------------------------------------------------------------------*/
;*    dump-hint ...                                                    */
;*---------------------------------------------------------------------*/
(define (dump-hint this)
   (if (or (>= (bigloo-debug) 2)
	   (string-contains (or (getenv "HOPTRACE") "") "j2s:hint"))
       (cond
	  ((isa? this J2SDecl)
	   (with-access::J2SDecl this (hint)
	      (if (pair? hint) `(:hint ,hint) '())))
	  ((isa? this J2SExpr)
	   (with-access::J2SExpr this (hint)
	      (if (pair? hint) `(:hint ,hint) '()))))
       '()))

;*---------------------------------------------------------------------*/
;*    dump-range ...                                                   */
;*---------------------------------------------------------------------*/
(define (dump-range this::J2SNode)
   (if (or (>= (bigloo-debug) 2)
	   (string-contains (or (getenv "HOPTRACE") "") "j2s:range"))
       (let ((range (cond
		      ((isa? this J2SExpr)
		       (with-access::J2SExpr this (range) range))
		      ((isa? this J2SDecl)
		       (with-access::J2SDecl this (range) range))
		      (else
		       '()))))
	  (if (not (eq? range #unspecified)) `(:range ,range) '()))
       '()))

;*---------------------------------------------------------------------*/
;*    dump-cache ...                                                   */
;*---------------------------------------------------------------------*/
(define (dump-cache this::J2SExpr)
   (if (or (>= (bigloo-debug) 2)
	   (string-contains (or (getenv "HOPTRACE") "") "j2s:cache"))
       (cond
	  ((isa? this J2SAccess)
	   (with-access::J2SAccess this (cache cspecs)
	      (if cache `(:cache ,cache :cspecs ,cspecs) '())))
	  ((isa? this J2SCall)
	   (with-access::J2SCall this (cache cspecs)
	      (if cache `(:cache ,cache :cspecs ,cspecs) '())))
	  ((isa? this J2SNew)
	   (with-access::J2SNew this (cache)
	      (if cache `(:cache ,cache) '())))
	  ((isa? this J2SAssig)
	   (with-access::J2SAssig this (lhs rhs)
	      (append (dump-cache lhs) (dump-cache rhs))))
	  (else
	   '()))
       '()))

;*---------------------------------------------------------------------*/
;*    dump-access ...                                                  */
;*---------------------------------------------------------------------*/
(define (dump-access this::J2SDecl)
   (if (or (>= (bigloo-debug) 2)
	   (string-contains (or (getenv "HOPTRACE") "") "j2s:access")
	   (string-contains (or (getenv "HOPTRACE") "") "j2s:usage"))
       (with-access::J2SDecl this (usecnt useinloop useinfun usage ronly writable scope)
	  `((:ronly ,ronly)
	    (:writable ,writable)
	    (:usecnt ,usecnt)
	    (:useinloop ,useinloop)
	    (:useinfun ,useinfun)
	    (:usage ,usage)
	    (:scope ,scope)))
       '()))

;*---------------------------------------------------------------------*/
;*    dump-key ...                                                     */
;*---------------------------------------------------------------------*/
(define (dump-key key)
   (if (or (>= (bigloo-debug) 2)
	   (string-contains  (or (getenv "HOPTRACE") "") "j2s:key"))
       `(:key ,key)
       '()))

;*---------------------------------------------------------------------*/
;*    dump-loc ...                                                     */
;*---------------------------------------------------------------------*/
(define (dump-loc loc #!optional (key :loc))
   (if (and loc
	    (or (>= (bigloo-debug) 2)
		(string-contains (or (getenv "HOPTRACE") "") "j2s:loc")))
       `(,key ,loc)
       '()))

;*---------------------------------------------------------------------*/
;*    dump-from ...                                                    */
;*---------------------------------------------------------------------*/
(define (dump-from from)
   (if (or (>= (bigloo-debug) 2)
	   (string-contains (or (getenv "HOPTRACE") "") "j2s:from"))
       `(:from ,(cond
		   ((isa? from J2SDecl)
		    (with-access::J2SDecl from (id key)
		       (format "[j2sdecl:~a,~a]" id key)))
		   ((isa? from J2SFun)
		    (with-access::J2SFun from (name loc)
		       (format "[j2sfun:~a,~a]" name loc)))
		   ((boolean? from)
		    from)
		   (else
		    (typeof from))))
       '()))

;*---------------------------------------------------------------------*/
;*    j2s-dump-decls ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-dump-decls decls)
   
   (define (dump-decl d)
      (cond
	 ((isa? d J2SDecl)
	  (with-access::J2SDecl d (id key)
	     (cons (format "~a[~a]" id key) (typeof d))))
	 ((isa? d J2SInit)
	  (with-access::J2SInit d (lhs)
	     (with-access::J2SRef lhs (decl)
		(with-access::J2SDecl decl (id key)
		   (cons (format "~a[~a]" id key) (typeof d))))))
	 (else
	  (typeof d))))
   
   (cond
      ((pair? decls) (map dump-decl decls))
      ((null? decls) '())
      (else (dump-decl decls))))

;*---------------------------------------------------------------------*/
;*    j2s-dump ::obj ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (j2s-dump this::obj)
   (call-with-output-file "/tmp/DUMP.scm"
      (lambda (op)
	 (pp (j2s->list this) op))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::obj ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (j2s->list this)
   (if (isa? this J2SNode)
       `(,(string->symbol (typeof this))
	 ,@(if (or (string? this) (symbol? this) (struct? this) (boolean? this)
		   (number? this) (char? this))
	       (list (format "~a" this))
	       '()))
       `(alien :typeof ,(string->symbol (typeof this))
	   :expr ,@(if (or (string? this) (symbol? this)
			   (struct? this) (boolean? this)
			   (number? this) (char? this))
		       (list (format "~a" this))
		       '()))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SStmt ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SStmt)
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SMeta ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SMeta)
   (with-access::J2SMeta this (stmt optim debug)
      `(,(string->symbol (typeof this)) :optim ,optim :debug ,debug
	  ,(j2s->list stmt))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SProgram ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SProgram)
   (with-access::J2SProgram this (nodes headers decls mode)
      `(,(string->symbol (typeof this))
	mode: ,mode
	headers: ,(map j2s->list headers)
	decls: ,(map j2s->list decls)
	nodes: ,(map j2s->list nodes))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SSeq ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SSeq)
   (with-access::J2SSeq this (nodes loc)
      `(,@(call-next-method)
	  ,@(dump-loc loc)
	  ,@(dump-info this)
	  ,@(map j2s->list nodes))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SBlock ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SBlock)
   (with-access::J2SBlock this (nodes loc endloc)
      `(,(string->symbol (typeof this))
	  ,@(dump-loc loc)
	  ,@(dump-loc endloc :endloc)
	  ,@(dump-info this)
	  ,@(map j2s->list nodes))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SLetBlock ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SLetBlock)
   (with-access::J2SLetBlock this (decls nodes loc endloc rec)
      `(,(string->symbol (typeof this)) :rec ,rec
	,@(dump-loc loc)
	,@(dump-loc endloc :endloc)
	,@(dump-info this)
	,(map j2s->list decls) ,@(map j2s->list nodes))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SVarDecls ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SVarDecls)
   (with-access::J2SVarDecls this (decls)
      `(,@(call-next-method) ,@(map j2s->list decls))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SAssig ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SAssig)
   (with-access::J2SAssig this (lhs rhs loc)
      `(,@(call-next-method) 
	  ,@(dump-loc loc)
	  ,@(dump-type this)
	  ,@(dump-range this)
	  ,@(dump-cache this)
	  ,@(if (isa? lhs J2SRef)
		(with-access::J2SRef lhs (decl)
		   (dump-vtype decl))
		'())
	  ,@(dump-info this)
	  ,(j2s->list lhs)
	  ,(j2s->list rhs))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SAssigOp ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SAssigOp)
   (with-access::J2SAssigOp this (lhs rhs loc op)
      `(,@(call-next-method) ,op)))
  
;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SPrefix ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SPrefix)
   (with-access::J2SPrefix this (lhs rhs loc op)
      `(,@(call-next-method) ,op)))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SPostfix ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SPostfix)
   (with-access::J2SPostfix this (lhs rhs loc op)
      `(,@(call-next-method) ,op)))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SUnresolvedRef ...                                 */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SUnresolvedRef)
   (with-access::J2SUnresolvedRef this (id loc)
      `(,@(call-next-method) ,(dump-loc loc) ,id
	  ,@(dump-type this))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SGlobalRef ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SGlobalRef)
   (with-access::J2SGlobalRef this (id decl loc)
      (with-access::J2SDecl decl (id usage)
	 `(,@(call-next-method)
	     ,@(dump-vtype decl)
	     ,@(dump-access decl)))))
 
;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SCast ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SCast)
   (with-access::J2SCast this (expr type)
      `(,@(call-next-method) ,type :from ,(j2s-type expr) ,(j2s->list expr))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SRef ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SRef)
   (with-access::J2SRef this (decl loc)
      (with-access::J2SDecl decl (id key)
	 `(,@(call-next-method) ,id
	     ,@(dump-loc loc)
	     ,@(dump-key key)
	     ,@(dump-type this)
	     ,@(dump-vtype decl)
	     ,@(dump-info this)
	     ,@(dump-hint this)
	     ,@(dump-range this)))))
 
;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SWithRef ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SWithRef)
   (with-access::J2SWithRef this (id withs expr type)
      `(,@(call-next-method) ,id ,@(dump-type this) ,withs ,(j2s->list expr))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SLiteral ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SLiteral)
   `(,@(call-next-method) ,@(dump-type this) ,@(dump-info this)))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SLiteralValue ...                                  */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SLiteralValue)
   (with-access::J2SLiteralValue this (val)
      `(,@(call-next-method) ,val)))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SLiteralCnst ...                                   */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SLiteralCnst)
   (with-access::J2SLiteralCnst this (val)
      `(,@(call-next-method)
	  ,@(dump-type this)
	  ,(j2s->list val))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SString ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SString)
   (with-access::J2SLiteralValue this (val)
      `(,(string->symbol (typeof this)) ,(format "~a" val))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SNativeString ...                                  */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SNativeString)
   (with-access::J2SLiteralValue this (val)
      `(,(string->symbol (typeof this)) ,(format "~a" val))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SArray ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SArray)
   (with-access::J2SArray this (exprs)
      `(,@(call-next-method) ,@(map j2s->list exprs))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SFun ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SFun)
   (with-access::J2SFun this (name thisp params body decl mode rtype optimize
				need-bind-exit-return idthis generator loc)
      (cond
	 ((isa? decl J2SDeclFun)
	  (with-access::J2SDecl decl (key usage scope)
	     `(,@(call-next-method) ,@(if generator '(*) '())
		 :name ,name
		 ,@(dump-key key)
		 ,@(dump-scope scope)
		 ,@(dump-access decl)
		 ,@(dump-info this)
		 ,@(dump-type this)
		 ,@(dump-rtype this)
		 ,@(if (>= (bigloo-debug) 3)
		       `(:need-bind-exit-return ,need-bind-exit-return) '())
		 :optimize ,optimize
		 :mode ,mode
		 ,@(dump-range this)
		 :thisp ,(j2s->list thisp)
		 ,(map j2s->list params) ,(j2s->list body))))
	 ((isa? decl J2SDecl)
	  (with-access::J2SDecl decl (key scope)
	     `(,@(call-next-method) ,@(if generator '(*) '())
		 :name ,name
		 ,@(dump-key key)
		 ,@(dump-scope scope)
		 ,@(dump-access decl)
		 ,@(dump-info this)
		 ,@(dump-type this)
		 ,@(dump-rtype this)
		 ,@(if (>= (bigloo-debug) 3)
		       `(:need-bind-exit-return ,need-bind-exit-return) '())
		 :optimize ,optimize
		 :mode ,mode
		 ,@(dump-range this)
		 :thisp ,(j2s->list thisp)
		 ,(map j2s->list params) ,(j2s->list body))))
	 (else
	  `(,@(call-next-method) ,@(if generator '(*) '())
	      :name ,name
	      ,@(dump-info this)
	      ,@(dump-type this)
	      ,@(dump-rtype this)
	      ,@(dump-loc loc)
	      ,@(if (>= (bigloo-debug) 3)
		    `(:need-bind-exit-return ,need-bind-exit-return) '())
	      :optimize ,optimize
	      :mode ,mode
	      ,@(dump-range this)
	      :thisp ,(j2s->list thisp)
	      ,(map j2s->list params) ,(j2s->list body))))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SMethod ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SMethod)
   (with-access::J2SMethod this (function method)
      `(,@(call-next-method)
	:function ,(j2s->list function)
	:method ,(j2s->list method))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SBindExit ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SBindExit)
   (with-access::J2SBindExit this (lbl stmt)
      `(,@(call-next-method)
	  ,@(dump-type this)
	  (,lbl) ,(j2s->list stmt))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SReturn ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SReturn)
   (with-access::J2SReturn this (expr tail from)
      `(,@(call-next-method)
	  ,@(dump-from from)
	  ,@(if (isa? from J2SFun)
		(dump-rtype from)
		'())
	  ,@(if (isa? from J2SBindExit)
		(with-access::J2SBindExit from (lbl type)
		   (list :from lbl :type type))
		'())
	  ,@(if (> (bigloo-debug) 2) `(:tail ,tail) '())
	  ,(j2s->list expr))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SReturnYield ...                                   */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SReturnYield)
   (with-access::J2SReturnYield this (expr kont)
      `(,@(call-next-method) ,(j2s->list expr)
	  ,(j2s->list kont))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SWith ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SWith)
   (with-access::J2SWith this (obj block)
      `(,@(call-next-method) ,(j2s->list obj) ,(j2s->list block))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SThrow ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SThrow)
   (with-access::J2SThrow this (expr)
      `(,@(call-next-method) ,(j2s->list expr))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2STry ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2STry)
   (with-access::J2STry this (body catch finally)
      `(,@(call-next-method) ,(j2s->list body)
	  ,(j2s->list catch)
	  ,(j2s->list finally))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SCatch ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SCatch)
   (with-access::J2SCatch this (param body)
      `(,@(call-next-method) ,(j2s->list param) ,(j2s->list body))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SBinary ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SBinary)
   (with-access::J2SBinary this (op rhs lhs loc)
      `(,@(call-next-method) ,op
	  ,@(dump-loc loc)
	  ,@(dump-type this)
	  ,@(dump-info this)
	  ,@(dump-hint this)
	  ,@(dump-range this)
	  ,(j2s->list lhs) ,(j2s->list rhs))))
      
;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SParen ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SParen)
   (with-access::J2SParen this (expr)
      `(,@(call-next-method)
	  ,@(dump-type this)
	  ,(j2s->list expr))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SUnary ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SUnary)
   (with-access::J2SUnary this (op expr)
      `(,@(call-next-method) ,op
	  ,@(dump-type this)
	  ,@(dump-info this)
	  ,@(dump-hint this)
	  ,(j2s->list expr))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SIf ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SIf)
   (with-access::J2SIf this (test then else)
      `(,@(call-next-method)
	  ,@(dump-info this)
	  ,(j2s->list test)
	  ,(j2s->list then)
	  ,(j2s->list else) )))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SPrecache ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SPrecache)
   (with-access::J2SPrecache this (accesses test then else)
      `(,(class-name (object-class this))
	  ,@(dump-info this)
	  ,(map j2s->list accesses)
	  ,(j2s->list then)
	  ,(j2s->list else) )))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SCond ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SCond)
   (with-access::J2SCond this (test then else)
      `(,@(call-next-method) ,@(dump-info this)
	  ,@(dump-type this)
	  ,@(dump-range this)
	  ,(j2s->list test)
	  ,(j2s->list then)
	  ,(j2s->list else) )))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SWhile ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SWhile)
   (with-access::J2SWhile this (op test body)
      `(,@(call-next-method) ,@(dump-info this)
	  ,(j2s->list test) ,(j2s->list body))))
   
;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SFor ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SFor)
   (with-access::J2SFor this (init test incr body)
      `(,@(call-next-method) ,@(dump-info this)
	  ,(j2s->list init)
	  ,(j2s->list test)
	  ,(j2s->list incr)
	  ,(j2s->list body))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SForIn ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SForIn)
   (with-access::J2SForIn this (lhs obj body)
      `(,@(call-next-method) ,@(dump-info this)
	  ,(j2s->list lhs)
	  ,(j2s->list obj)
	  ,(j2s->list body))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SLabel ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SLabel)
   (with-access::J2SLabel this (lhs id body)
      `(,@(call-next-method) ,(j2s->list id)
	  ,(j2s->list body))))
   
;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SBreak ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SBreak)
   (with-access::J2SBreak this (loc id target)
      `(,@(call-next-method) ,id ,(typeof target))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SContinue ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SContinue)
   (with-access::J2SContinue this (loc id)
      `(,@(call-next-method) ,id)))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SCall ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SCall)
   (with-access::J2SCall this (fun thisarg args loc)
      `(,@(call-next-method)
	  ,@(dump-loc loc)
	  ,@(dump-type this)
	  ,@(dump-info this)
	  ,@(dump-range this)
	  ,(j2s->list fun)
	  ,@(dump-cache this)
	  ,@(if (pair? thisarg) `(:thisarg ,@(map j2s->list thisarg)) '())
	  ,@(map j2s->list args))))
		  
;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SAccess ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SAccess)
   (with-access::J2SAccess this (obj field loc)
      `(,@(call-next-method)
	  ,@(dump-loc loc)
	  ,@(dump-type this)
	  ,@(dump-info this)
	  ,@(dump-hint this)
	  ,@(dump-range this)
	  ,@(dump-cache this)
	  ,(j2s->list obj) ,(j2s->list field))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SCacheCheck ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SCacheCheck)
   (with-access::J2SCacheCheck this (cache obj fields)
      `(,@(call-next-method) :cache ,cache
	  ,(j2s->list obj)
	  ,@(map j2s->list fields))))
   
;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SStmtExpr ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SStmtExpr)
   (with-access::J2SStmtExpr this (expr)
      `(,@(call-next-method) ,@(dump-info this) ,(j2s->list expr))))
   
;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SObjectInit ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SObjInit)
   (with-access::J2SObjInit this (inits)
      `(,@(call-next-method) ,@(map j2s->list inits))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SDataPropertyInit ...                              */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SDataPropertyInit)
   (with-access::J2SDataPropertyInit this (name val)
      `(,@(call-next-method) ,(j2s->list name) ,(j2s->list val))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SAccessorPropertyInit ...                          */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SAccessorPropertyInit)
   (with-access::J2SAccessorPropertyInit this (name get set)
      `(,@(call-next-method) ,(j2s->list name)
	  :get ,(j2s->list get) :set ,(j2s->list set))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SDecl ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SDecl)
   (with-access::J2SDecl this (id key binder _scmid usage ronly scope cache)
      `(,(string->symbol (format "~a/~a" (typeof this) binder))
	,id
	,@(dump-dump this)
	,@(dump-key key)
	,@(dump-access this)
	,@(dump-itype this)
	,@(dump-hint this)
	,@(dump-range this)
	,@(if _scmid `(:_scmid ,_scmid) '())
	,@(dump-info this)
	,@(dump-scope scope))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SDeclInit ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SDeclInit)
   (with-access::J2SDeclInit this (val ronly writable val scope id)
      `(,@(call-next-method)
	  ,@(if (> (bigloo-debug) 2) `(:ronly ,ronly) '())
	  ,@(if (> (bigloo-debug) 2) `(:writable ,writable) '())
	  ,@(if (> (bigloo-debug) 3) `(:scope ,scope) '())
	  ,@(if (nodefval? val) '() (list (j2s->list val))))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SPragma ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SPragma)
   (with-access::J2SPragma this (expr vars vals type)
      `(,@(call-next-method) ,@(dump-type this)
	  :vars ,vars
	  :vals ,(map j2s->list vals)
	  ',expr)))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SSequence ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SSequence)
   (with-access::J2SSequence this (exprs)
      `(,@(call-next-method) ,@(dump-type this) ,@(map j2s->list exprs))))
		  
;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SNew ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SNew)
   (with-access::J2SNew this (clazz args cache)
      `(,@(call-next-method) ,@(dump-type this)
	  ,@(dump-cache this)
	  ,(j2s->list clazz) ,@(map j2s->list args))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SYield ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SYield)
   (with-access::J2SYield this (expr)
      `(,@(call-next-method) ,(j2s->list expr))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SKont ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SKont)
   (with-access::J2SKont this (param exn body)
      `(,@(call-next-method) ,(j2s->list param) ,(j2s->list exn) ,(j2s->list body))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SHopRef ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SHopRef)
   (with-access::J2SHopRef this (id module)
      `(,@(call-next-method) ,id ,@(dump-type this)
	  ,@(if module (list module) '()))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2Stilde ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2STilde)
   (with-access::J2STilde this (stmt)
      `(,@(call-next-method) ,(j2s->list stmt))))
      
;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SDollar ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SDollar)
   (with-access::J2SDollar this (node)
      `(,@(call-next-method) ,(j2s->list node))))
      
;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SSwitch ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SSwitch)
   (with-access::J2SSwitch this (key cases)
      `(,@(call-next-method) ,(j2s->list key)
	  ,@(map j2s->list cases))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SCase ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SCase)
   (with-access::J2SCase this (expr body cascade)
      (append (call-next-method)
	 (if (>= (bigloo-debug) 3) `(:cascade ,cascade) '())
	 (list (j2s->list expr) (j2s->list body)))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SDefault ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SDefault)
   (with-access::J2SDefault this (body)
      (list 'J2SDefault (j2s->list body))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SDeclClass ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SDeclClass)
   (with-access::J2SDeclClass this (val key ronly writable val scope id)
      `(,@(call-next-method)
	  ,@(dump-key key)
	  ,@(if (> (bigloo-debug) 2) `(:ronly ,ronly) '())
	  ,@(if (> (bigloo-debug) 2) `(:writable ,writable) '())
	  ,@(if (> (bigloo-debug) 3) `(:scope ,scope) '())
	  ,@(if (nodefval? val) '() (list (j2s->list val))))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SClass ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SClass)
   (with-access::J2SClass this (name super elements decl)
      `(J2SClass ,@(if name (list :name name) '())
	  :super ,(j2s->list super)
	  ,@(if (isa? decl J2SDecl)
		(with-access::J2SDecl decl (key)
		   (dump-key key))
		'())
	  ,@(map j2s->list elements))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SClassElement ...                                  */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SClassElement)
   (with-access::J2SClassElement this (prop static)
      `(J2SClassElement :static ,static
	  ,(j2s->list prop))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SDProducer ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SDProducer)
   (with-access::J2SDProducer this (decl expr)
      `(,@(call-next-method) ,@(dump-type this) ,(j2s->list expr))))
		  
;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SDConsumer ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SDConsumer)
   (with-access::J2SDConsumer this (expr path)
      `(,@(call-next-method) ,@(dump-type this) ,path ,(j2s->list expr))))

