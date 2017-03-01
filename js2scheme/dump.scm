;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/dump.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 11 11:12:21 2013                          */
;*    Last change :  Mon Feb 27 13:14:19 2017 (serrano)                */
;*    Copyright   :  2013-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Dump the AST for debugging                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_dump
   
   (import __js2scheme_ast
	   __js2scheme_stage)

   (export j2s-dump-stage
	   (j2s-dump-decls::obj ::obj)
	   (generic j2s->list ::obj)))

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
   
   (define (info this)
      (cond
	 ((and (pair? this) (null? (cdr this)))
	  (cons (info (car this)) (info (cdr this))))
	 ((list? this)
	  (map info this))
	 ((or (string? this) (symbol? this) (number? this) (boolean? this))
	  this)
	 ((or (char? this) (eq? this #unspecified))
	  this)
	 ((struct? this)
	  this)
	 (else
	  (format "[[~a]]" (typeof this)))))
   
   (with-access::J2SNode this (%info)
      (if (or (>= (bigloo-debug) 3)
	      (string-contains (or (getenv "HOPTRACE") "") "j2s:info"))
	  `(:%info ,(info %info))
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
(define (dump-hint this::J2SNode)
   (if (or (>= (bigloo-debug) 2)
	   (string-contains (or (getenv "HOPTRACE") "") "j2s:hint"))
       (let ((hint (cond
		      ((isa? this J2SExpr)
		       (with-access::J2SExpr this (hint) hint))
		      ((isa? this J2SDecl)
		       (with-access::J2SDecl this (hint) hint))
		      (else
		       '()))))
	  (if (pair? hint) `(:hint ,hint) '()))
       '()))

;*---------------------------------------------------------------------*/
;*    dump-access ...                                                  */
;*---------------------------------------------------------------------*/
(define (dump-access this::J2SDecl)
   (if (or (>= (bigloo-debug) 2)
	   (string-contains (or (getenv "HOPTRACE") "") "j2s:access")
	   (string-contains (or (getenv "HOPTRACE") "") "j2s:usage"))
       (with-access::J2SDecl this (usecnt usage ronly)
	  `((:ronly ,ronly)
	    (:usecnt ,usecnt)
	    (:usage ,usage)))
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
   `(,(string->symbol (typeof this))
     ,@(if (or (string? this) (symbol? this) (struct? this) (boolean? this)
               (number? this) (char? this))
           (list (format "~a" this))
           '())))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SStmt ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SStmt)
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SProgram ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SProgram)
   (with-access::J2SProgram this (nodes headers decls)
      `(,(string->symbol (typeof this))
	headers: ,(map j2s->list headers)
	decls: ,(map j2s->list decls)
	nodes: ,(map j2s->list nodes))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SSeq ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SSeq)
   (with-access::J2SSeq this (nodes)
      `(,@(call-next-method) ,@(map j2s->list nodes))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SLetBlock ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SLetBlock)
   (with-access::J2SLetBlock this (decls nodes)
      `(,(string->symbol (typeof this))
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
	  ,@(dump-type this)
	  ,@(dump-info this)
	  ,(j2s->list lhs)
	  ,(j2s->list rhs))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SAssigOp ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SAssigOp)
   (with-access::J2SAssigOp this (lhs rhs loc op)
      `(,(call-next-method) ,op ,@(dump-type this))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SPrefix ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SPrefix)
   (with-access::J2SPrefix this (lhs rhs loc op)
      `(,@(call-next-method) ,op ,@(dump-type this))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SPostfix ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SPostfix)
   (with-access::J2SPostfix this (lhs rhs loc op)
      `(,@(call-next-method) ,op ,@(dump-type this))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SUnresolvedRef ...                                 */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SUnresolvedRef)
   (with-access::J2SUnresolvedRef this (id)
      `(,@(call-next-method) ,id
	  ,@(dump-type this))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SCast ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SCast)
   (with-access::J2SCast this (expr type)
      `(,@(call-next-method) ,type ,(j2s->list expr))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SRef ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SRef)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (id key)
	 `(,@(call-next-method) ,id
	     ,@(dump-key key)
	     ,@(dump-type this)
	     ,@(dump-info this)))))
 
;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SWithRef ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SWithRef)
   (with-access::J2SWithRef this (id withs expr)
      `(,@(call-next-method) ,id ,withs ,(j2s->list expr))))

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

   (define (dump-rtype this)
      (with-access::J2SFun this (rtype)
	 (if (or (>= (bigloo-debug) 2)
		 (string-contains  (or (getenv "HOPTRACE") "")
		    "j2s:type"))
	     `(:rtype ,rtype)
	     '())))
      
   (with-access::J2SFun this (name thisp params body decl mode rtype
				need-bind-exit-return idthis generator)
      (cond
	 ((or (isa? decl J2SDeclFun) (isa? decl J2SDeclFunCnst))
	  (with-access::J2SDecl decl (key usage)
	     `(,@(call-next-method) ,@(if generator '(*) '())
		 :name ,name
		 ,@(dump-key key)
		 ,@(dump-rtype this)
		 ,@(if (>= (bigloo-debug) 3)
		       `(:idthis ,idthis) '())
		 ,@(if (>= (bigloo-debug) 3)
		       `(:usage ,usage) '())
		 ,@(if (>= (bigloo-debug) 3)
		       `(:need-bind-exit-return ,need-bind-exit-return) '())
		 :mode ,mode
		 ,(j2s->list thisp) ,(map j2s->list params) ,(j2s->list body))))
	 ((isa? decl J2SDecl)
	  (with-access::J2SDecl decl (key)
	     `(,@(call-next-method) ,@(if generator '(*) '())
		 :name ,name
		 ,@(dump-key key)
		 ,@(dump-rtype this)
		 ,@(if (>= (bigloo-debug) 3)
		       `(:idthis ,idthis) '())
		 ,@(if (>= (bigloo-debug) 3)
		       `(:need-bind-exit-return ,need-bind-exit-return) '())
		 :mode ,mode
		 ,(j2s->list thisp) ,(map j2s->list params) ,(j2s->list body))))
	 (else
	  `(,@(call-next-method) ,@(if generator '(*) '())
	      :name ,name
	      ,@(dump-rtype this)
	      ,@(if (>= (bigloo-debug) 3)
		    `(:idthis ,idthis) '())
	      ,@(if (>= (bigloo-debug) 3)
		    `(:need-bind-exit-return ,need-bind-exit-return) '())
	      ,(j2s->list thisp) ,(map j2s->list params) ,(j2s->list body))))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SReturn ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SReturn)
   (with-access::J2SReturn this (expr tail)
      `(,@(call-next-method)
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
   (with-access::J2SBinary this (op rhs lhs)
      `(,@(call-next-method) ,op
	  ,@(dump-type this)
	  ,@(dump-info this)
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
	  ,(j2s->list expr))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SIf ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SIf)
   (with-access::J2SIf this (test then else)
      `(,@(call-next-method) ,(j2s->list test)
	  ,(j2s->list then)
	  ,(j2s->list else) )))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SCond ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SCond)
   (with-access::J2SCond this (test then else)
      `(,@(call-next-method) ,(j2s->list test)
	  ,(j2s->list then)
	  ,(j2s->list else) )))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SComprehension ...                                 */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SComprehension)
   (with-access::J2SComprehension this (decls iterables test expr)
      `(,@(call-next-method) 
	  (,@(map j2s->list decls) of ,@(map j2s->list iterables))
	  (if ,(j2s->list test))
	  ,(j2s->list expr))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SWhile ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SWhile)
   (with-access::J2SWhile this (op test body)
      `(,@(call-next-method) ,(j2s->list test) ,(j2s->list body))))
   
;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SFor ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SFor)
   (with-access::J2SFor this (init test incr body)
      `(,@(call-next-method) ,(j2s->list init)
	  ,(j2s->list test)
	  ,(j2s->list incr)
	  ,(j2s->list body))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SForIn ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SForIn)
   (with-access::J2SForIn this (lhs obj body)
      `(,@(call-next-method) ,(j2s->list lhs)
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
   (with-access::J2SCall this (fun args (cthis this))
      `(,@(call-next-method)
	  ,@(dump-type this)
	  ,@(dump-info this)
	  ,@(if (>=fx (bigloo-debug) 3) `(:this ,(j2s->list cthis)) '())
	  ,(j2s->list fun) ,@(map j2s->list args))))
		  
;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SAccess ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SAccess)
   (with-access::J2SAccess this (obj field)
      `(,@(call-next-method)
	  ,@(dump-type this)
	  ,@(dump-info this)
	  ,(j2s->list obj) ,(j2s->list field))))
   
;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SStmtExpr ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SStmtExpr)
   (with-access::J2SStmtExpr this (expr)
      `(,@(call-next-method) ,(j2s->list expr))))
   
;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SExprStmt ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SExprStmt)
   (with-access::J2SExprStmt this (stmt)
      `(,@(call-next-method) ,(j2s->list stmt))))
   
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
	  ,(j2s->list get) ,(j2s->list set))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SDecl ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SDecl)
   (with-access::J2SDecl this (id key binder _scmid usecnt usage ronly scope)
      `(,(string->symbol (format "~a/~a" (typeof this) binder))
	,id
	,@(dump-key key)
	,@(dump-access this)
	,@(dump-itype this)
	,@(dump-hint this)
	,@(if _scmid `(:_scmid ,_scmid) '())
	,@(dump-info this)
	,@(if (>= (bigloo-debug) 3) `(:scope ,scope) '()))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SDeclInit ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SDeclInit)
   (with-access::J2SDeclInit this (val ronly writable val _scmid scope id)
      `(,@(call-next-method)
	  ,@(if (> (bigloo-debug) 2) `(:ronly ,ronly) '())
	  ,@(if (> (bigloo-debug) 2) `(:writable ,writable) '())
	  ,@(if (> (bigloo-debug) 3) `(:scope ,scope) '())
	  ,@(if (nodefval? val) '() (list (j2s->list val))))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SPragma ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SPragma)
   (with-access::J2SPragma this (expr)
      `(,@(call-next-method) ',expr)))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SSequence ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SSequence)
   (with-access::J2SSequence this (exprs)
      `(,@(call-next-method) ,@(map j2s->list exprs))))
		  
;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SNew ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SNew)
   (with-access::J2SNew this (clazz args)
      `(,@(call-next-method) ,(j2s->list clazz) ,@(map j2s->list args))))

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
      `(,@(call-next-method) ,id ,@(if module (list module) '()))))

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

