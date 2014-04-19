;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/ast.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 11 08:54:57 2013                          */
;*    Last change :  Sat Apr 19 08:46:00 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JavaScript AST                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_ast

   (export (abstract-class J2SNode
	      (loc::pair read-only))

	   (abstract-class J2SStmt::J2SNode)

	   (class J2SSeq::J2SStmt
	      nodes::pair-nil)
	   
	   (class J2SBlock::J2SSeq)

	   (class J2SProgram::J2SBlock
	      (mode::symbol read-only (default 'normal))
	      (path::bstring read-only)
	      (pcache-size::int (default 0))
	      (name read-only (default #f))
	      (main read-only (default #f))
	      (module read-only (default #f)))

	   (final-class J2SStmtExpr::J2SStmt
	      expr::J2SExpr)

	   (final-class J2SIf::J2SStmt
	      test::J2SExpr
	      then::J2SStmt
	      else::J2SStmt)
	   
	   (final-class J2SVarDecls::J2SStmt
	      decls::pair)

	   (class J2SIdStmt::J2SStmt
	      (need-bind-exit-break::bool (default #t))
	      (id::obj read-only (default #unspecified)))
	   
	   (final-class J2SSwitch::J2SIdStmt
	      key::J2SNode
	      cases::pair-nil)

	   (class J2SLoop::J2SIdStmt
	      (need-bind-exit-continue::bool (default #t))
	      body::J2SStmt)
	      
	   (final-class J2SFor::J2SLoop
	      init::J2SNode
	      test::J2SExpr
	      incr::J2SExpr)

	   (final-class J2SForIn::J2SLoop
	      lhs::J2SNode
	      obj::J2SExpr)

	   (class J2SWhile::J2SLoop
	      test::J2SExpr)

	   (class J2SDo::J2SWhile)

	   (final-class J2SLabel::J2SStmt
	      id
	      body::J2SStmt)
	   
	   (final-class J2SBreak::J2SStmt
	      (target (default #f))
	      (id (default #f)))
	   
	   (final-class J2SContinue::J2SStmt
	      (target (default #f))
	      (id (default #f)))
	   
	   (final-class J2SNop::J2SStmt)

	   (class J2SCase::J2SStmt
	      expr::J2SExpr
	      body::J2SSeq)

	   (final-class J2SDefault::J2SCase)

	   (final-class J2SReturn::J2SStmt
	      (tail::bool (default #t))
	      expr::J2SExpr)

	   (final-class J2SWith::J2SStmt
	      (id::symbol read-only (default (gensym '__with)))
	      obj::J2SExpr
	      block::J2SStmt)
	   
	   (final-class J2SThrow::J2SStmt
	      expr::J2SExpr)

	   (class J2SFun::J2SExpr
	      (mode read-only (default #f))
	      (id read-only (default #f))
	      (need-bind-exit-return::bool (default #f))
	      (vararg::bool (default #f))
	      params::pair-nil
	      body::J2SNode)

	   (class J2SSvc::J2SFun
	      (init::J2SNode read-only))
	   
	   (final-class J2SCatch::J2SStmt
	      param::J2SParam
	      body::J2SNode)

	   (final-class J2STry::J2SStmt
	      body::J2SStmt
	      catch::J2SStmt
	      finally::J2SStmt)
	   
	   (abstract-class J2SExpr::J2SNode)

	   (class J2SPragma::J2SExpr
	      (expr read-only))

	   (class J2SSequence::J2SExpr
	      (exprs::pair read-only))
	   
	   (class J2SUnresolvedRef::J2SExpr
	      (cache (default #f))
	      id)

	   (class J2SRef::J2SExpr
	      decl::J2SDecl)

	   (class J2SWithRef::J2SExpr
	      (id::symbol read-only)
	      withs::pair
	      (expr::J2SExpr read-only))

	   (class J2SHopRef::J2SExpr
	      (id::symbol read-only)
	      (module read-only (default #f)))

	   (final-class J2SThis::J2SExpr)
	   
	   (final-class J2SCond::J2SExpr
	      test::J2SExpr
	      then::J2SExpr
	      else::J2SExpr)
	   
	   (class J2SDecl::J2SNode
	      (id::symbol read-only)
	      (name (default #f))
	      (writable (default #t))
	      (ronly (default #f))
	      (global::bool (default #f))
	      (use::int (default 0))
	      (type::obj (default #unspecified)))
	   
	   (class J2SDeclInit::J2SDecl
	      (val::J2SExpr read-only))
	   
	   (class J2SDeclFun::J2SDeclInit)
	   
	   (class J2SDeclSvc::J2SDeclFun)
	   
	   (final-class J2SDeclExtern::J2SDeclInit
	      (bind::bool read-only (default #f)))

	   (final-class J2SParam::J2SDecl)
	   
	   (abstract-class J2SLiteral::J2SExpr)
	   
	   (final-class J2SArrayAbsent::J2SLiteral)
	   
	   (final-class J2SNull::J2SLiteral)
	   (final-class J2SUndefined::J2SLiteral)

	   (class J2SLiteralValue::J2SLiteral
	      val)

	   (class J2SString::J2SLiteralValue
	      (escape::pair-nil read-only (default '())))
	   (final-class J2SBool::J2SLiteralValue)
	   (class J2SNumber::J2SLiteralValue)
	   (final-class J2SOctalNumber::J2SNumber)
	   (final-class J2SRegExp::J2SLiteralValue
	      (flags::bstring read-only))

	   (final-class J2SArray::J2SLiteral
	      len::int
	      exprs::pair-nil)
	      
	   (class J2SUnary::J2SExpr
	      op::symbol
	      expr::J2SExpr)

	   (class J2SAssig::J2SExpr
	      lhs::J2SExpr
	      rhs::J2SExpr)
	   
	   (final-class J2SPrefix::J2SAssig
	      op::symbol)
	   (final-class J2SPostfix::J2SAssig
	      op::symbol)

	   (final-class J2SBinary::J2SExpr
	      op::symbol
	      lhs::J2SExpr
	      rhs::J2SExpr)

	   (final-class J2SAccess::J2SExpr
	      (cache (default #f))
	      obj::J2SExpr
	      field::J2SExpr)

	   (final-class J2SCall::J2SExpr
	      fun::J2SExpr
	      (args::pair-nil (default '())))

	   (final-class J2SXml::J2SExpr
	      tag::symbol
	      (attrs::pair-nil (default '()))
	      body::J2SExpr)

	   (final-class J2STilde::J2SExpr
	      stmt::J2SStmt)
	   
	   (final-class J2SDollar::J2SExpr
	      expr::J2SExpr)
	   
	   (final-class J2SNew::J2SExpr
	      clazz::J2SNode
	      args::pair-nil)

	   (class J2SInit::J2SAssig)

	   (final-class J2SAssigOp::J2SAssig
	      op::symbol)
	   
	   (final-class J2SVAssig::J2SAssig)
	   (final-class J2SCAssig::J2SAssig)
	   
	   (final-class J2SFunBinding::J2SInit)

	   (final-class J2SObjInit::J2SExpr
	      inits::pair-nil)

	   (abstract-class J2SPropertyInit::J2SNode
	      name::J2SLiteralValue)
	   
	   (final-class J2SDataPropertyInit::J2SPropertyInit
	      val::J2SExpr)

	   (final-class J2SAccessorPropertyInit::J2SPropertyInit
	      (get::obj (default #f))
	      (set::obj (default #f)))

	   (generic walk0 n::J2SNode p::procedure)
	   (generic walk1 n::J2SNode p::procedure a0)
	   (generic walk2 n::J2SNode p::procedure a0 a1)
	   (generic walk3 n::J2SNode p::procedure a0 a1 a2)
	   (generic walk4 n::J2SNode p::procedure a0 a1 a2 a3)
	   (generic walk5 n::J2SNode p::procedure a0 a1 a2 a3 a4)
	   (generic walk0*::pair-nil n::J2SNode p::procedure)
	   (generic walk1*::pair-nil n::J2SNode p::procedure a0)
	   (generic walk2*::pair-nil n::J2SNode p::procedure a0 a1)
	   (generic walk3*::pair-nil n::J2SNode p::procedure a0 a1 a2)
	   (generic walk4*::pair-nil n::J2SNode p::procedure a0 a1 a2 a3)
	   (generic walk5*::pair-nil n::J2SNode p::procedure a0 a1 a2 a3 a4)
	   (generic walk0!::J2SNode n::J2SNode p::procedure)
	   (generic walk1!::J2SNode n::J2SNode p::procedure a0)
	   (generic walk2!::J2SNode n::J2SNode p::procedure a0 a1)
	   (generic walk3!::J2SNode n::J2SNode p::procedure a0 a1 a2)
	   (generic walk4!::J2SNode n::J2SNode p::procedure a0 a1 a2 a3)
	   (generic walk5!::J2SNode n::J2SNode p::procedure a0 a1 a2 a3 a4)
	   
	   (macro define-walk-method)))

;*---------------------------------------------------------------------*/
;*    define-walk-method ...                                           */
;*    -------------------------------------------------------------    */
;*    (define-walk-method (optim! this::While x y) BODY)               */
;*    =>                                                               */
;*    (define-method (optim! this::While x y)                          */
;*      (define (default-walk! n x y)                                  */
;*         (walk2! n optim! x y))                                      */
;*      (define (walk! n x y)                                          */
;*         (optim! n x y))                                             */
;*      BODY)                                                          */
;*---------------------------------------------------------------------*/
(define-macro (define-walk-method args . body)
   
   (define (parse-ident id::symbol)
      (let* ((string (symbol->string id))
	     (len (string-length string)))
	 (let loop ((walker  0))
	    (cond
	       ((=fx walker len)
		(values id #f))
	       ((and (char=? (string-ref string walker) #\:)
		     (<fx walker (-fx len 1))
		     (char=? (string-ref string (+fx walker 1)) #\:))
		(values (string->symbol (substring string 0 walker))
		   (string->symbol (substring string (+fx walker 2)))))
	       (else
		(loop (+fx walker 1)))))))

   (define (id-without-type id)
      (multiple-value-bind (id type)
	 (parse-ident id)
	 id))

   (define (id-type id)
      (multiple-value-bind (id type)
	 (parse-ident id)
	 type))

   (let* ((name (car args))
	  (sname (symbol->string name))
	  (i (string-contains sname "::"))
	  (c (string-ref sname (-fx (or i (string-length sname)) 1)))
	  (nb-method-args (-fx (length args) 2))
	  (short-walk (case c
			 ((#\!) 'walk!)
			 ((#\*) 'walk*)
			 (else 'walk)))
	  (tname (case c
		    ((#\!) (string->symbol (string-append sname "::J2SNode")))
		    ((#\*) (string->symbol (string-append sname "::pair-nil")))
		    (else name)))
	  (long-walk (case c
			((#\!)
			 (string->symbol (format "walk~a!" nb-method-args)))
			((#\*)
			 (string->symbol (format "walk~a*" nb-method-args)))
			(else
			 (string->symbol (format "walk~a" nb-method-args)))))
	  (define-gen/met (if (eq? (id-type (cadr args)) 'J2SNode)
			      'define-generic
			      'define-method))
	  (default-walk (symbol-append 'default- short-walk)))
      `(,define-gen/met	(,tname ,@(cdr args))
	  (define (call-default-walker)
	     (,long-walk ,(id-without-type (cadr args)) ,(id-without-type name)
		,@(map id-without-type (cddr args))))
	  (define (,default-walk ,(id-without-type (cadr args)) ,@(cddr args))
	     (,long-walk ,(id-without-type (cadr args)) ,(id-without-type name)
		,@(map id-without-type (cddr args))))
	  (define (,short-walk ,(id-without-type (cadr args)) ,@(cddr args))
	     (,name ,@(map id-without-type (cdr args))))
	  ,@body)))

;*---------------------------------------------------------------------*/
;*    generic walks ...                                                */
;*---------------------------------------------------------------------*/
(define-generic (walk0 n::J2SNode p::procedure)
   (error "walk0" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk1 n::J2SNode p::procedure arg0)
   (error "walk1" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk2 n::J2SNode p::procedure arg0 arg1)
   (error "walk2" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk3 n::J2SNode p::procedure arg0 arg1 arg2)
   (error "walk3" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk4 n::J2SNode p::procedure arg0 arg1 arg2 arg3)
   (error "walk4" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk5 n::J2SNode p::procedure arg0 arg1 arg2 arg3 arg4)
   (error "walk5" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))

(define-generic (walk0*::pair-nil n::J2SNode p::procedure)
   (error "walk0*" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk1*::pair-nil n::J2SNode p::procedure arg0)
   (error "walk1*" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk2*::pair-nil n::J2SNode p::procedure arg0 arg1)
   (error "walk2*" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk3*::pair-nil n::J2SNode p::procedure arg0 arg1 arg2)
   (error "walk3*" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk4*::pair-nil n::J2SNode p::procedure arg0 arg1 arg2 arg3)
   (error "walk4!" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk5*::pair-nil n::J2SNode p::procedure arg0 arg1 arg2 arg3 arg4)
   (error "walk5!" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))

(define-generic (walk0!::J2SNode n::J2SNode p::procedure)
   (error "walk0!" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk1!::J2SNode n::J2SNode p::procedure arg0)
   (error "walk1!" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk2!::J2SNode n::J2SNode p::procedure arg0 arg1)
   (error "walk2!" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk3!::J2SNode n::J2SNode p::procedure arg0 arg1 arg2)
   (error "walk3!" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk4!::J2SNode n::J2SNode p::procedure arg0 arg1 arg2 arg3)
   (error "walk4!" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk5!::J2SNode n::J2SNode p::procedure arg0 arg1 arg2 arg3 arg4)
   (error "walk5!" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))

;*---------------------------------------------------------------------*/
;*    gen-walks ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (gen-walks class . fields)
   
   (define (field-name f)
      (if (pair? f)
	  (car f)
	  f))
   
   (define (visit f nb-args)
      (if (pair? f)
	  `(for-each (lambda (f)
			(p f 
			   ,@(map (lambda (i)
				     (string->symbol (format "arg~a" i)))
				(iota nb-args))))
	      ,(car f))
	  `(p ,f ,@(map (lambda (i)
			   (string->symbol (format "arg~a" i)))
		      (iota nb-args)))))
   
   (define (visit* f nb-args)
      (if (pair? f)
	  `(append-map (lambda (f)
			  (p f 
			     ,@(map (lambda (i)
				       (string->symbol (format "arg~a" i)))
				  (iota nb-args))))
	      ,(car f))
	  `(p ,f ,@(map (lambda (i)
			   (string->symbol (format "arg~a" i)))
		      (iota nb-args)))))
   
   (define (visit! f nb-args)
      (if (pair? f)
	  `(let loop ((fields ,(car f)))
	      (unless (null? fields)
		 (set-car! fields
		    (p (car fields)
		       ,@(map (lambda (i)
				 (string->symbol (format "arg~a" i)))
			    (iota nb-args))))
		 (loop (cdr fields))))
	  `(set! ,f (p ,f ,@(map (lambda (i)
				    (string->symbol (format "arg~a" i)))
			       (iota nb-args))))))

   (define (withaccess body)
      `(,(symbol-append 'with-access:: class) n ,(map field-name fields)
           ;;; the body of the with-access form
	   ,body))
      
   (define (gen-method nb-args)
      `(define-method (,(string->symbol (format "walk~a" nb-args))
		       ,(symbol-append 'n:: class)
		       p
		       ,@(map (lambda (i)
				 (string->symbol (format "arg~a" i)))
			    (iota nb-args)))
	  ,(when (pair? fields)
	     (withaccess
		`(begin ,@(map (lambda (f) (visit f nb-args)) fields))))))

   (define (gen-method* nb-args)
      `(define-method (,(string->symbol (format "walk~a*" nb-args))
		       ,(symbol-append 'n:: class)
		       p
		       ,@(map (lambda (i)
				 (string->symbol (format "arg~a" i)))
			    (iota nb-args)))
	  ,(if (null? fields)
	       ''()
	       (withaccess
		  `(append ,@(map (lambda (f) (visit* f nb-args)) fields))))))

   (define (gen-method! nb-args)
      `(define-method (,(string->symbol (format "walk~a!" nb-args))
		       ,(symbol-append 'n:: class)
		       p
		       ,@(map (lambda (i)
				 (string->symbol (format "arg~a" i)))
			    (iota nb-args)))
	  ,(when (pair? fields)
	     (withaccess
		`(begin
		    ,@(map (lambda (f) (visit! f nb-args)) fields))))
	  n))
   
   `(begin
       ,@(map (lambda (nb) (gen-method nb)) (iota 5))
       ,@(map (lambda (nb) (gen-method* nb)) (iota 5))
       ,@(map (lambda (nb) (gen-method! nb)) (iota 5))))

;*---------------------------------------------------------------------*/
;*    gen-traverals ...                                                */
;*---------------------------------------------------------------------*/
(define-macro (gen-traverals class)

   (define (gen-method nb-args)
      (let ((args (map (lambda (i)
			  (string->symbol (format "arg~a" i)))
		     (iota nb-args)))
	    (walk (string->symbol (format "walk~a" nb-args))))
	 `(define-method (,walk ,(symbol-append 'n:: class) p ,@args)
	     (let loop ((n n))
		(cond
		   ((isa? n J2SDollar)
		    (,walk n p ,@args))
		   ((isa? n J2SNode)
		    (let ((fields (class-all-fields (object-class n))))
		       (let for ((i (-fx (vector-length fields) 1)))
			  (when (>=fx i 0)
			     (let ((f (vector-ref fields i)))
				(unless (eq? (class-field-name f) 'loc)
				   (let ((v ((class-field-accessor f) n)))
				      (loop v)))
				(for (-fx i 1)))))))
		   ((pair? n)
		    (for-each loop n)))))))

   (define (gen-method* nb-args)
      (let ((args (map (lambda (i)
			  (string->symbol (format "arg~a" i)))
		     (iota nb-args)))
	    (walk (string->symbol (format "walk~a*" nb-args))))
	 `(define-method (,walk ,(symbol-append 'n:: class) p ,@args)
	     (let loop ((n n))
		(cond
		   ((isa? n J2SDollar)
		    (,walk n p ,@args))
		   ((isa? n J2SNode)
		    (let ((fields (class-all-fields (object-class n))))
		       (let for ((i (-fx (vector-length fields) 1)))
			  (if (=fx i -1)
			      '()
			      (let ((f (vector-ref fields i)))
				 (if (eq? (class-field-name f) 'loc)
				     (for (-fx i 1))
				     (let ((v ((class-field-accessor f) n)))
					(append (loop v) (for (-fx i 1))))))))))
		   ((pair? n)
		    (append-map loop n))
		   (else
		    '()))))))
   
   (define (gen-method! nb-args)
      (let ((args (map (lambda (i)
			  (string->symbol (format "arg~a" i)))
		     (iota nb-args)))
	    (walk (string->symbol (format "walk~a!" nb-args))))
	 `(define-method (,walk ,(symbol-append 'n:: class) p ,@args)
	     (let loop ((n n))
		(cond
		   ((isa? n J2SDollar)
		    (,walk n p ,@args))
		   ((isa? n J2SNode)
		    (let ((fields (class-all-fields (object-class n))))
		       (let for ((i (-fx (vector-length fields) 1)))
			  (if (>=fx i 0)
			      (let ((f (vector-ref fields i)))
				 (unless (eq? (class-field-name f) 'loc)
				    (let ((v ((class-field-accessor f) n)))
				       ((class-field-mutator f) n (loop v))))
				 (for (-fx i 1)))
			      n))))
		   ((pair? n)
		    (map! loop n))
		   (else
		    n))))))
   
   `(begin
       ,@(map (lambda (nb) (gen-method nb)) (iota 5))
       ,@(map (lambda (nb) (gen-method* nb)) (iota 5))
       ,@(map (lambda (nb) (gen-method! nb)) (iota 5))))

;*---------------------------------------------------------------------*/
;*    default walk                                                     */
;*---------------------------------------------------------------------*/
(gen-walks J2SNode)
(gen-walks J2SSeq (nodes))
(gen-walks J2SReturn expr)
(gen-walks J2SWith obj block)
(gen-walks J2SThrow expr)
(gen-walks J2STry body catch finally)
(gen-walks J2SCatch body)
(gen-walks J2SStmtExpr expr)
(gen-walks J2SSequence (exprs))
(gen-walks J2SVarDecls (decls))
(gen-walks J2SAssig lhs rhs)
(gen-walks J2SSwitch key (cases))
(gen-walks J2SLabel body)
(gen-walks J2SFor init test incr body)
(gen-walks J2SForIn lhs obj body)
(gen-walks J2SWhile test body)
(gen-walks J2SCase expr body)
(gen-walks J2SUnary expr)
(gen-walks J2SBinary lhs rhs)
(gen-walks J2SDefault body)
(gen-walks J2SAccess obj field)
(gen-walks J2SCall fun (args))
(gen-walks J2SXml (attrs) body)
(gen-walks J2SNew clazz (args))
(gen-walks J2SAssig lhs rhs)
(gen-walks J2SFun body)
(gen-walks J2SSvc body init)
(gen-walks J2SObjInit (inits))
(gen-walks J2SDataPropertyInit name val)
(gen-walks J2SAccessorPropertyInit name get set)
(gen-walks J2SArray (exprs))
(gen-walks J2SDeclInit val)
(gen-walks J2SWithRef expr)
(gen-walks J2SIf test then else)
(gen-walks J2SCond test then else)
(gen-walks J2SDollar expr)

(gen-traverals J2STilde)
