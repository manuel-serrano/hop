;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/ast.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 11 08:54:57 2013                          */
;*    Last change :  Tue May  1 16:12:50 2018 (serrano)                */
;*    Copyright   :  2013-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JavaScript AST                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_ast
   
   (library web)
   
   (include "walk.sch")

   (export (abstract-class J2SNode
	      (loc::pair read-only (info '("notraverse")))
	      (%info (default #unspecified) (info '("notraverse")))
	      (%%dump (default #unspecified) (info '("notraverse")))
	      (%%wstamp (default -1) (info '("notraverse"))))
	   
	   (abstract-class J2SStmt::J2SNode)

	   (class J2SMeta::J2SStmt
	      (debug::long (default 0))
	      (optim::long (default 0))
	      (stmt::J2SStmt (info '("ast"))))
	   
	   (class J2SSeq::J2SStmt
	      (nodes::pair-nil (info '("ast"))))
	   
	   (class J2SBlock::J2SSeq
	      (endloc::pair read-only (info '("notraverse"))))
	   
	   (class J2SProgram::J2SBlock
	      (version::int read-only (default 1))
	      (mode::symbol read-only (default 'normal))
	      (path::bstring read-only)
	      (pcache-size::int (default 0))
	      (name read-only (default #f))
	      (main read-only (default #f))
	      (module read-only (default #f))
	      (cnsts::pair-nil (default '()))
	      (decls::pair-nil (default '()) (info '("ast")))
	      (headers::pair-nil (default '()) (info '("ast")))
	      (globals::pair-nil (default '()) (info '("ast")))
	      (direct-eval::bool (default #t))
	      (source-map (default #f)))

	   (abstract-class J2SExpr::J2SNode
	      (type::symbol (default 'unknown) (info '("notraverse")))
	      (hint::pair-nil (default '()) (info '("notraverse")))
	      (range::obj (default #unspecified) (info '("notraverse"))))

	   (class J2SCast::J2SExpr
	      (expr::J2SExpr (info '("ast"))))
	   
	   (final-class J2SStmtExpr::J2SStmt
	      (expr::J2SExpr (info '("ast"))))
	   
	   (class J2SIf::J2SStmt
	      (test::J2SExpr (info '("ast")))
	      (then::J2SStmt (info '("ast")))
	      (else::J2SStmt (info '("ast"))))

	   (final-class J2SPrecache::J2SIf
	      (accesses::pair-nil read-only (default '())))
	   
	   (final-class J2SVarDecls::J2SStmt
	      (decls::pair (info '("ast"))))

	   (final-class J2SLetBlock::J2SBlock
	      (rec::bool (default #t))
	      (decls::pair-nil (info '("ast"))))
	   
	   (class J2SIdStmt::J2SStmt
	      (need-bind-exit-break::bool (default #t))
	      (id::obj (default #unspecified)))
	   
	   (final-class J2SSwitch::J2SIdStmt
	      (key::J2SExpr (info '("ast")))
	      (cases::pair-nil (info '("ast"))))
	   
	   (class J2SLoop::J2SIdStmt
	      (need-bind-exit-continue::bool (default #t))
	      (body::J2SStmt (info '("ast"))))
	   
	   (final-class J2SFor::J2SLoop
	      (init::J2SNode (info '("ast")))
	      (test::J2SExpr (info '("ast")))
	      (incr::J2SExpr (info '("ast"))))
	   
	   (final-class J2SForIn::J2SLoop
	      ;; op: in, of
	      (op::symbol read-only (default 'in))
	      (lhs::J2SNode (info '("ast")))
	      (obj::J2SExpr (info '("ast"))))
	   
	   (final-class J2SForOf::J2SLoop
	      (lhs::J2SNode (info '("ast")))
	      (obj::J2SExpr (info '("ast"))))
	   
	   (class J2SWhile::J2SLoop
	      (test::J2SExpr (info '("ast"))))
	   
	   (class J2SDo::J2SWhile)
	   
	   (final-class J2SLabel::J2SIdStmt
	      (body::J2SStmt (info '("ast"))))
	   
	   (class J2SBreak::J2SStmt
	      (target (default #f) (info '("notraverse")))
	      (id (default #f)))
	   
	   (final-class J2SContinue::J2SBreak)
	   
	   (final-class J2SNop::J2SStmt)
	   
	   (class J2SCase::J2SStmt
	      (expr::J2SExpr (info '("ast")))
	      (body::J2SSeq (info '("ast")))
	      (cascade::bool (default #f)))
	   
	   (final-class J2SDefault::J2SCase)
	   
	   (final-class J2SBindExit::J2SExpr
	      (lbl read-only)
	      (stmt::J2SStmt (info '("ast"))))

	   (class J2SReturn::J2SStmt
	      (exit::bool (default #f))
	      (tail::bool (default #t))
	      (from (default #unspecified) (info '("notraverse")))
	      (expr::J2SExpr (info '("ast"))))
	   
	   (class J2SReturnYield::J2SStmt
	      (expr::J2SExpr (info '("ast")))
	      (generator::bool read-only (default #f))
	      (kont::J2SExpr (info '("ast"))))
	   
	   (final-class J2SYield::J2SExpr
	      (expr::J2SExpr (info '("ast")))
	      (generator::bool read-only (default #f)))
	   
	   (final-class J2SWith::J2SStmt
	      (id::symbol read-only (default (gensym '__with)))
	      (obj::J2SExpr (info '("ast")))
	      (block::J2SStmt (info '("ast"))))
	   
	   (final-class J2SThrow::J2SStmt
	      (expr::J2SExpr (info '("ast"))))
	   
	   (class J2SFun::J2SExpr
	      (rtype::symbol (default 'unknown) (info '("notraverse")))
	      (idthis (default 'this) (info '("notraverse")))
	      (idgen read-only (default #f) (info '("notraverse")))
	      (mode::symbol (default 'normal) (info '("notraverse")))
	      (decl (default #f) (info '("notraverse")))
	      (need-bind-exit-return::bool (default #f) (info '("notraverse")))
	      (vararg::obj (default #f) (info '("notraverse")))
	      (name read-only (default '||) (info '("notraverse")))
	      (generator::bool (default #f) (info '("notraverse")))
	      (optimize (default #t) (info '("notraverse")))
	      (thisp (default #f) (info '("notraverse")))
	      (params::pair-nil (default '()))
	      (constrsize::int (default 3) (info '("notraverse")))
	      (src::bool (default #t) (info '("notraverse")))
	      (method (default #f) (info '("notraverse")))
	      (ismethodof (default #f) (info '("notraverse")))
	      (body::J2SBlock (info '("ast"))))
	   
	   (class J2SSvc::J2SFun
	      init::J2SNode
	      (path::obj read-only (default #f) (info '("notraverse")))
	      (register::bool read-only (default #t) (info '("notraverse")))
	      (import::bool read-only (info '("notraverse"))))

	   (class J2SArrow::J2SFun)

	   (class J2SMethod::J2SExpr
	      (function::J2SFun (info '("ast")))
	      (method::J2SFun (info '("ast"))))

	   (class J2SClass::J2SExpr
	      (endloc::pair read-only (info '("notraverse")))
	      (name read-only (info '("notraverse")))
	      (decl (default #f) (info '("notraverse")))
	      (super::J2SExpr (info '("ast")))
	      (src::bool (default #t) (info '("notraverse")))
	      (elements::pair-nil (info '("ast"))))

	   (class J2SClassElement::J2SNode
	      (static::bool read-only)
	      (prop::J2SPropertyInit (info '("ast"))))
	   
	   (final-class J2SCatch::J2SStmt
	      param::J2SDecl
	      (body::J2SNode (info '("ast"))))
	   
	   (final-class J2STry::J2SStmt
	      (body::J2SBlock (info '("ast")))
	      (catch::J2SStmt (info '("ast")))
	      (finally::J2SStmt (info '("ast"))))
	   
	   (class J2SPragma::J2SExpr
	      (lang::symbol (default 'scheme))
	      (expr (info '("ast"))))
	   
	   (class J2SSequence::J2SExpr
	      (exprs::pair (info '("ast"))))
	   
	   (class J2SUnresolvedRef::J2SExpr
	      (cache (default #f))
	      id::symbol)
	   
	   (class J2SGlobalRef::J2SUnresolvedRef
	      (decl::J2SDecl (info '("nojson"))))
	   
	   (class J2SRef::J2SExpr
	      (decl::J2SDecl (info '("nojson"))))
	   
	   (class J2SWithRef::J2SExpr
	      (id::symbol read-only)
	      withs::pair
	      (expr::J2SExpr (info '("ast"))))

	   (class J2SHopRef::J2SExpr
	      (id::symbol read-only)
	      (itype::symbol (default 'any))
	      (rtype::symbol (default 'any))
	      (vtype::symbol (default 'any))
	      (module read-only (default #f)))

	   (class J2SLetRef::J2SRef)
	   
	   (final-class J2SAref::J2SRef
	      (array::J2SDecl read-only)
	      (alen::J2SDecl read-only)
	      (amark::obj read-only)
	      (deps::pair-nil read-only))
	   
	   (final-class J2SThis::J2SRef)
	   
	   (final-class J2SSuper::J2SRef
	      (clazz (default #unspecified)))

	   (final-class J2SCond::J2SExpr
	      (test::J2SExpr (info '("ast")))
	      (then::J2SExpr (info '("ast")))
	      (else::J2SExpr (info '("ast"))))

	   (class J2SDecl::J2SStmt
	      id::symbol
	      (_scmid (default #f) (info '("notraverse")))
	      (key (default (ast-decl-key)) (info '("notraverse")))
	      ;; writable=#f iff decl is const
	      (writable (default #t) (info '("notraverse")))
	      (immutable (default #f) (info '("notraverse")))
	      (ronly (default #f) (info '("notraverse")))
	      (scope::symbol (default 'local) (info '("notraverse")))
	      (usecnt::int (default 0) (info '("notraverse")))
	      (useinloop::bool (default #f) (info '("notraverse")))
	      (useinfun::bool (default #f) (info '("notraverse")))
	      ;; usage: init, new, ref, assig, get (field), set (field), call
	      (usage::pair-nil (default '()) (info '("notraverse")))
	      (utype::symbol (default 'unknown) (info '("notraverse")))
	      (itype::symbol (default 'unknown) (info '("notraverse")))
	      (vtype::symbol (default 'unknown) (info '("notraverse")))
	      (hint::pair-nil (default '()) (info '("notraverse")))
	      (range::obj (default #unspecified) (info '("notraverse")))
	      (binder::symbol (default 'var) (info '("notraverse"))))
	   
	   (class J2SDeclInit::J2SDecl
	      (val::J2SExpr (info '("ast"))))

	   (class J2SDeclFun::J2SDeclInit
	      (parent read-only (default #f))
	      (expression::bool (default #f))
	      (hintinfo::obj (default #f) (info '("notraverse"))))

	   (class J2SDeclFunType::J2SDeclFun)

	   (class J2SDeclClass::J2SDecl
	      (val::J2SClass (info '("ast"))))

	   (class J2SDeclSvc::J2SDeclFun)

	   (final-class J2SDeclExtern::J2SDeclInit
	      (bind::bool read-only (default #f)))

	   (abstract-class J2SLiteral::J2SExpr)

	   (final-class J2SArrayAbsent::J2SLiteral)
	   (final-class J2SDots::J2SLiteral
	      id)
	   
	   (final-class J2SNull::J2SLiteral)
	   (final-class J2SUndefined::J2SLiteral)

	   (class J2SLiteralValue::J2SLiteral
	      val)

	   (class J2SNativeString::J2SLiteralValue)
	   
	   (class J2SString::J2SLiteralValue
	      (escape::pair-nil read-only (default '())))
	   (final-class J2SBool::J2SLiteralValue)
	   (class J2SNumber::J2SLiteralValue)
	   (final-class J2SOctalNumber::J2SNumber)
	   (final-class J2SRegExp::J2SLiteralValue
	      (flags::bstring read-only)
	      (inline::bool (default #f)))
	   (final-class J2SCmap::J2SLiteralValue)

	   (final-class J2SLiteralCnst::J2SLiteral
	      (index::long read-only)
	      (val::J2SLiteralValue read-only (info '("notraverse"))))
	       
	   (final-class J2SArray::J2SLiteral
	      len::int
	      (exprs::pair-nil (info '("ast"))))
	   
	   (final-class J2STemplate::J2SExpr
	      (exprs::pair read-only (info '("ast"))))
	   
	   (final-class J2SParen::J2SExpr
	      (expr::J2SExpr (info '("ast"))))
	   
	   (class J2SUnary::J2SExpr
	      op::symbol
	      (expr::J2SExpr (info '("ast"))))
	   
	   (final-class J2SBinary::J2SExpr
	      op::symbol
	      (lhs::J2SExpr (info '("ast")))
	      (rhs::J2SExpr (info '("ast"))))
	   
	   (class J2SAssig::J2SExpr
	      (lhs::J2SExpr (info '("ast")))
	      (rhs::J2SExpr (info '("ast"))))
	   
	   (class J2SInit::J2SAssig)
	   
	   (final-class J2SVAssig::J2SAssig)
	   (final-class J2SCAssig::J2SAssig)

	   (final-class J2SFunBinding::J2SInit)
	   
	   (final-class J2SPrefix::J2SAssig
	      op::symbol)
	   (final-class J2SPostfix::J2SAssig
	      op::symbol)
	   (final-class J2SAssigOp::J2SAssig
	      op::symbol)
	   
	   (final-class J2SObjInit::J2SExpr
	      (inits::pair-nil (info '("ast")))
	      (cmap (default #f)))
	   
	   (final-class J2SAccess::J2SExpr
	      (cache (default #f) (info '("notraverse")))
	      (cspecs (default '(imap emap cmap pmap amap vtable)) (info '("notraverse")))
	      (obj::J2SExpr (info '("ast")))
	      (field::J2SExpr (info '("ast"))))

	   (final-class J2SCacheCheck::J2SExpr
	      (prop::symbol read-only)
	      (cache read-only (info '("notraverse")))
	      (obj::J2SExpr (info '("ast")))
	      fields::pair-nil)

	   (final-class J2SCacheUpdate::J2SExpr
	      (prop::symbol read-only)
	      (cache read-only (info '("notraverse")))
	      (obj::J2SExpr (info '("ast"))))

	   (final-class J2SCall::J2SExpr
	      (cache (default #f) (info '("notraverse")))
	      (cspecs (default '(pmap vtable)) (info '("notraverse")))
	      (fun::J2SExpr (info '("ast")))
	      (protocol (default 'direct) (info '("notraverse")))
	      (thisarg::pair-nil (info '("ast")))
	      (args::pair-nil (default '()) (info '("ast"))))
	   
	   (final-class J2STilde::J2SExpr
	      stmt::J2SStmt)
	   
	   (final-class J2SDollar::J2SExpr
	      (node::J2SNode (info '("ast"))))
	   
	   (final-class J2SNew::J2SExpr
	      (cache (default #f))
	      (clazz::J2SNode (info '("ast")))
	      (args::pair-nil (info '("ast"))))

	   (abstract-class J2SPropertyInit::J2SNode
	      (name::J2SExpr (info '("ast"))))
	   
	   (final-class J2SDataPropertyInit::J2SPropertyInit
	      (val::J2SExpr (info '("ast"))))
	   
	   (final-class J2SAccessorPropertyInit::J2SPropertyInit
	      (get::obj (default #f) (info '("ast")))
	      (set::obj (default #f) (info '("ast"))))

	   (final-class J2SKont::J2SExpr
	      (param::J2SDecl read-only)
	      (exn::J2SDecl read-only)
	      (body::J2SNode (info '("ast"))))

	   (final-class J2SOPTInitSeq::J2SSeq
	      ref::J2SRef
	      (cmap0::symbol read-only)
	      (cmap1::symbol read-only)
	      (offset::symbol read-only))

	   (final-class J2SDProducer::J2SExpr
	      (decl::J2SDecl (info '("nojson")))
	      (expr::J2SExpr (info '("ast")))
	      (size::long read-only (info '("notraverse"))))

	   (final-class J2SDConsumer::J2SExpr
	      (decl::J2SDecl (info '("nojson")))
	      (expr::J2SExpr (info '("ast")))
	      (path read-only))

	   (generic walk0 n::J2SNode p::procedure)
	   (generic walk1 n::J2SNode p::procedure a0)
	   (generic walk2 n::J2SNode p::procedure a0 a1)
	   (generic walk3 n::J2SNode p::procedure a0 a1 a2)
	   (generic walk4 n::J2SNode p::procedure a0 a1 a2 a3)
	   (generic walk5 n::J2SNode p::procedure a0 a1 a2 a3 a4)
	   (generic walk6 n::J2SNode p::procedure a0 a1 a2 a3 a4 a5)
	   (generic walk7 n::J2SNode p::procedure a0 a1 a2 a3 a4 a5 a6)
	   (generic walk0*::pair-nil n::J2SNode p::procedure)
	   (generic walk1*::pair-nil n::J2SNode p::procedure a0)
	   (generic walk2*::pair-nil n::J2SNode p::procedure a0 a1)
	   (generic walk3*::pair-nil n::J2SNode p::procedure a0 a1 a2)
	   (generic walk4*::pair-nil n::J2SNode p::procedure a0 a1 a2 a3)
	   (generic walk5*::pair-nil n::J2SNode p::procedure a0 a1 a2 a3 a4)
	   (generic walk6*::pair-nil n::J2SNode p::procedure a0 a1 a2 a3 a4 a5)
	   (generic walk7*::pair-nil n::J2SNode p::procedure a0 a1 a2 a3 a4 a5 a6)
	   (generic walk0!::J2SNode n::J2SNode p::procedure)
	   (generic walk1!::J2SNode n::J2SNode p::procedure a0)
	   (generic walk2!::J2SNode n::J2SNode p::procedure a0 a1)
	   (generic walk3!::J2SNode n::J2SNode p::procedure a0 a1 a2)
	   (generic walk4!::J2SNode n::J2SNode p::procedure a0 a1 a2 a3)
	   (generic walk5!::J2SNode n::J2SNode p::procedure a0 a1 a2 a3 a4)
	   (generic walk6!::J2SNode n::J2SNode p::procedure a0 a1 a2 a3 a4 a5)
	   (generic walk7!::J2SNode n::J2SNode p::procedure a0 a1 a2 a3 a4 a5 a6)
	   
	   (macro define-walk-method)

	   (j2sfun-id ::J2SFun)
	   (j2sfun-expression? ::J2SFun)
	   
	   (ast-decl-key::int)
	   
	   (ast->json ::obj ::output-port)
	   (json->ast ::input-port)

	   (nodefval::J2SUndefined)
	   (nodefval?::bool ::J2SExpr)

	   (j2s-var?::bool ::J2SDecl)
	   (j2s-let?::bool ::J2SDecl)
	   (j2s-const?::bool ::J2SDecl)
	   (j2s-param?::bool ::J2SDecl)
	   
	   (j2s-let-opt?::bool ::J2SDecl)

	   (j2s-field-name::obj ::J2SNode)
	   (inline j2s-field-length?::bool ::J2SNode)

	   (j2sdeclinit-val-fun::J2SExpr ::J2SDeclInit))
   
   (static (class %JSONDecl::J2SDecl
	      (%id read-only))))

;*---------------------------------------------------------------------*/
;*    %nodefval ...                                                    */
;*---------------------------------------------------------------------*/
(define %nodefval
   (instantiate::J2SUndefined
      (loc '("no loc"))))

;*---------------------------------------------------------------------*/
;*    nodefval ...                                                     */
;*---------------------------------------------------------------------*/
(define (nodefval)
   %nodefval)

;*---------------------------------------------------------------------*/
;*    nodefval? ...                                                    */
;*---------------------------------------------------------------------*/
(define (nodefval? v)
   (eq? v %nodefval))

;*---------------------------------------------------------------------*/
;*    j2s-var? ...                                                     */
;*---------------------------------------------------------------------*/
(define (j2s-var? decl::J2SDecl)
   (with-access::J2SDecl decl (binder id loc)
      (case binder
	 ((var) #t)
	 ((let let-opt) #t)
	 ((param class) #f)
	 (else (error "j2s-var?" "wrong binder" (vector loc id binder))))))

;*---------------------------------------------------------------------*/
;*    j2s-let? ...                                                     */
;*---------------------------------------------------------------------*/
(define (j2s-let? decl::J2SDecl)
   (with-access::J2SDecl decl (binder id loc)
      (case binder
	 ((let let-opt) #t)
	 ((var param class) #f)
	 (else (error "j2s-let?" "wrong binder" (vector loc id binder))))))

;*---------------------------------------------------------------------*/
;*    j2s-const? ...                                                   */
;*---------------------------------------------------------------------*/
(define (j2s-const? decl::J2SDecl)
   (with-access::J2SDecl decl (binder writable id loc)
      (unless writable
	 (case binder
	    ((let let-opt) #t)
	    ((var param class) #f)
	    (else (error "j2s-const?" "wrong binder" (vector loc id binder)))))))

;*---------------------------------------------------------------------*/
;*    j2s-let-opt? ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-let-opt? decl::J2SDecl)
   (with-access::J2SDecl decl (binder id loc)
      (case binder
	 ((let-opt) #t)
	 ((let var param class) #f)
	 (else (error "j2s-let-opt?" "wrong binder" (vector loc id binder))))))

;*---------------------------------------------------------------------*/
;*    j2s-access-field ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-field-name field)
   (cond
      ((isa? field J2SLiteralCnst)
       (with-access::J2SLiteralCnst field (val)
	  (j2s-field-name val)))
      ((isa? field J2SString)
       (with-access::J2SString field (val)
	  val))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    j2s-field-length? ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (j2s-field-length? field)
   (equal? (j2s-field-name field) "length"))

;*---------------------------------------------------------------------*/
;*    j2s-param? ...                                                   */
;*---------------------------------------------------------------------*/
(define (j2s-param? decl::J2SDecl)
   (with-access::J2SDecl decl (binder id loc)
      (case binder
	 ((param) #t)
	 ((var let const let-opt const-opt class) #f)
	 (else (error "j2s-param?" "wrong binder" (vector loc id binder))))))

;*---------------------------------------------------------------------*/
;*    j2sfun-id ...                                                    */
;*---------------------------------------------------------------------*/
(define (j2sfun-id this::J2SFun)
   (with-access::J2SFun this (decl)
      (when (isa? decl J2SDecl)
	 (with-access::J2SDecl decl (id)
	    id))))

;*---------------------------------------------------------------------*/
;*    j2sfun-expression? ...                                           */
;*---------------------------------------------------------------------*/
(define (j2sfun-expression? this::J2SFun)
   (with-access::J2SFun this (decl)
      (when (isa? decl J2SDeclFun)
	 (with-access::J2SDeclFun decl (expression) expression))))

;*---------------------------------------------------------------------*/
;*    *ast-decl-key* ...                                               */
;*---------------------------------------------------------------------*/
(define *ast-decl-key* 0)

;*---------------------------------------------------------------------*/
;*    ast-decl-key ...                                                 */
;*---------------------------------------------------------------------*/
(define (ast-decl-key)
   (let ((v (+fx 1 *ast-decl-key*)))
      (set! *ast-decl-key* v)
      v))

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
(define-generic (walk6 n::J2SNode p::procedure arg0 arg1 arg2 arg3 arg4 arg5)
   (error "walk6" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk7 n::J2SNode p::procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6)
   (error "walk7" "Internal Error: forgot Node type"
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
   (error "walk5*" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk6*::pair-nil n::J2SNode p::procedure arg0 arg1 arg2 arg3 arg4 arg5)
   (error "walk6*" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk7*::pair-nil n::J2SNode p::procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6)
   (error "walk7*" "Internal Error: forgot Node type"
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
(define-generic (walk6!::J2SNode n::J2SNode p::procedure arg0 arg1 arg2 arg3 arg4 arg5)
   (error "walk6!" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk7!::J2SNode n::J2SNode p::procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6)
   (error "walk7!" "Internal Error: forgot Node type"
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

   ;; generate the JavaScript walker
   (cond-expand
      (generate-javascript-walker
       (printf "ast.~a.prototype.HopcWalk = function( ast ) {\n" class)
       (display "   var _this = this;\n")
       (display "   var _ast = ast;\n")
       ;; (printf "   console.log( 'walk ', '~a', _ast.__node__ );\n" class)
       (for-each (lambda (f)
		    (cond
		       ((symbol? f)
			(printf "   Array.prototype.splice.call( arguments, 0, 1, _ast.~a );\n" f)
			(printf "   _ast.~a = _this.walkNode.apply( _this, arguments );\n" f))
		       ((pair? f)
			(printf "   _ast.~a.forEach( function( v, i, a ) {\n" (car f))
			(printf "      Array.prototype.splice.call( arguments, 0, 1, v );\n" (car f))
			(printf "      a[ i ] = _this.walkNode.apply( _this, arguments );\n" (car f))
			(printf "   } );\n"))))
	  fields)
       (print "   return _ast;\n};\n\n")))
   
   `(begin
       ,@(map (lambda (nb) (gen-method nb)) (iota 8))
       ,@(map (lambda (nb) (gen-method* nb)) (iota 8))
       ,@(map (lambda (nb) (gen-method! nb)) (iota 8))))

;*---------------------------------------------------------------------*/
;*    gen-traversals ...                                               */
;*---------------------------------------------------------------------*/
(define-macro (gen-traversals class)

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
			     (let* ((f (vector-ref fields i))
				    (fi (class-field-info f)))
				(unless (and (pair? fi) (member "notraverse" fi))
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
			      (let* ((f (vector-ref fields i))
				     (fi (class-field-info f)))
				 (if (and (pair? fi) (member "notraverse" fi))
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
		    (,walk n p ,@args)
		    n)
		   ((isa? n J2SNode)
		    (let ((fields (class-all-fields (object-class n))))
		       (let for ((i (-fx (vector-length fields) 1)))
			  (if (>=fx i 0)
			      (let* ((f (vector-ref fields i))
				     (fi (class-field-info f)))
				 (unless (and (pair? fi) (member "notraverse" fi))
				    (let ((v ((class-field-accessor f) n)))
				       ((class-field-mutator f) n (loop v))))
				 (for (-fx i 1)))
			      n))))
		   ((pair? n)
		    (map! loop n))
		   (else
		    n))))))

   `(begin
       ,@(map (lambda (nb) (gen-method nb)) (iota 8))
       ,@(map (lambda (nb) (gen-method* nb)) (iota 8))
       ,@(map (lambda (nb) (gen-method! nb)) (iota 8))))

;*---------------------------------------------------------------------*/
;*    default walk                                                     */
;*---------------------------------------------------------------------*/
(gen-walks J2SNode)
(gen-walks J2SMeta stmt)
(gen-walks J2SSeq (nodes))
(gen-walks J2SProgram (decls) (headers) (nodes))
(gen-walks J2SBindExit stmt)
(gen-walks J2SReturn expr)
(gen-walks J2SReturnYield expr kont)
(gen-walks J2SWith obj block)
(gen-walks J2SThrow expr)
(gen-walks J2STry body catch finally)
(gen-walks J2SCatch body)
(gen-walks J2SStmtExpr expr)
;* (gen-walks J2SExprStmt stmt)                                        */
(gen-walks J2SSequence (exprs))
(gen-walks J2SVarDecls (decls))
(gen-walks J2SLetBlock (decls) (nodes))
(gen-walks J2SAssig lhs rhs)
(gen-walks J2SSwitch key (cases))
(gen-walks J2SLabel body)
(gen-walks J2SFor init test incr body)
(gen-walks J2SForIn lhs obj body)
(gen-walks J2SWhile test body)
(gen-walks J2SCase expr body)
(gen-walks J2SDefault body)
(gen-walks J2STemplate (exprs))
(gen-walks J2SParen expr)
(gen-walks J2SUnary expr)
(gen-walks J2SBinary lhs rhs)
(gen-walks J2SDefault body)
(gen-walks J2SAccess obj field)
(gen-walks J2SCacheCheck obj)
(gen-walks J2SCacheUpdate obj)
(gen-walks J2SCall fun (thisarg) (args))
(gen-walks J2SNew clazz (args))
(gen-walks J2SAssig lhs rhs)
(gen-walks J2SFun body (params))
(gen-walks J2SSvc body init (params))
(gen-walks J2SMethod function method)
(gen-walks J2SObjInit (inits))
(gen-walks J2SDataPropertyInit name val)
(gen-walks J2SAccessorPropertyInit name get set)
(gen-walks J2SArray (exprs))
(gen-walks J2SDeclInit val)
(gen-walks J2SWithRef expr)
(gen-walks J2SIf test then else)
(gen-walks J2SPrecache (accesses) test then else)
(gen-walks J2SCond test then else)
(gen-walks J2SDollar node)
(gen-walks J2SYield expr)
(gen-walks J2SKont body)
(gen-walks J2SCast expr)
(gen-walks J2SClass super (elements))
(gen-walks J2SClassElement prop)
(gen-walks J2SDProducer expr)
(gen-walks J2SDConsumer expr)

(gen-traversals J2STilde)

;*---------------------------------------------------------------------*/
;*    walk.sch at runtime ...                                          */
;*---------------------------------------------------------------------*/
(define-macro (eval-walker! path)
   (call-with-input-file path
      (lambda (p)
	 `(eval! ',(read p)))))

(eval-walker! "walk.sch")

;*---------------------------------------------------------------------*/
;*    ast->json ...                                                    */
;*---------------------------------------------------------------------*/
(define (ast->json ast op::output-port)
   (display "{ " op)
   (display "\"__ast__\": " op)
   (j2s->json ast op)
   (display " }" op))

;*---------------------------------------------------------------------*/
;*    j2s->json ::obj ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (j2s->json this::obj op::output-port)
   (match-case this
      ((at ?fname ?point)
       (display "\"" op)
       (display fname op)
       (display ":" op)
       (display point op)
       (display "\"" op))
      ((?- . ?-)
       (display "[" op)
       (let loop ((l this)
		  (sep ""))
	  (if (pair? l)
	      (begin
		 (display sep op)
		 (j2s->json (car l) op)
		 (loop (cdr l) ", "))
	      (display "]" op))))
      (()
       (display "[]" op))
      ((? string?)
       (display "\"" op)
       (display (string-for-read this) op)
       (display "\"" op))
      ((? symbol?)
       (display "{ \"__symbol__\": \"" op)
       (display this op)
       (display "\" }" op))
      (#unspecified
       (display "undefined" op))
      (#t
       (display "true" op))
      (#f
       (display "false" op))
      (else
       ;; other literal
       (display this op))))

;*---------------------------------------------------------------------*/
;*    j2s->json ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s->json this::J2SNode op::output-port)
   (let ((clazz (object-class this)))
      (fprintf op "{ \"__node__\": \"~a\"" (class-name clazz))
      (let ((fields (class-all-fields clazz)))
	 (let for ((i (-fx (vector-length fields) 1)))
	    (when (>=fx i 0)
	       (let* ((f (vector-ref fields i))
		      (fi (class-field-info f)))
		  (unless (and (pair? fi) (member "nojson" fi))
		     (fprintf op ", \"~a\": " (class-field-name f))
		     (let ((v ((class-field-accessor f) this)))
			(j2s->json v op))))
	       (for (-fx i 1)))))
      (display " }" op)))

;*---------------------------------------------------------------------*/
;*    j2s->json ::J2SRef ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s->json this::J2SRef op::output-port)
   (with-access::J2SRef this (decl)
      (let ((clazz (object-class this)))
	 (fprintf op "{ \"__node__\": \"~a\"" (class-name clazz))
	 (with-access::J2SDecl decl (key)
	    (fprintf op ", \"decl\": {\"__ref__\": ~a}" key))
	 (let ((fields (class-all-fields clazz)))
	    (let for ((i (-fx (vector-length fields) 1)))
	       (when (>=fx i 0)
		  (let* ((f (vector-ref fields i))
			 (fi (class-field-info f)))
		     (unless (and (pair? fi) (member "nojson" fi))
			(fprintf op ", \"~a\": " (class-field-name f))
			(let ((v ((class-field-accessor f) this)))
			   (j2s->json v op))))
		  (for (-fx i 1)))))
	 (display " }" op))))

;*---------------------------------------------------------------------*/
;*    j2s->json ::J2SPragma ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s->json this::J2SPragma op::output-port)
   (with-access::J2SPragma this (expr loc lang)
      (display "{ \"__node__\": \"J2SPragma\", \"expr\": \"" op)
      (display (string-for-read
		  (call-with-output-string
		     (lambda (op) (write expr op))))
	 op)
      (display "\", \"lang\": " op)
      (display #\" op)
      (display lang op)
      (display #\" op)
      (display "\", \"loc\": " op)
      (j2s->json loc op)
      (display " }" op)))

;*---------------------------------------------------------------------*/
;*    json->ast ...                                                    */
;*---------------------------------------------------------------------*/
(define (json->ast ip::input-port)

   (define (alist->node cname l)
      (let* ((clazz (find-class cname))
	     (ctor (class-constructor clazz))
	     (inst ((class-allocator clazz)))
	     (fields (class-all-fields clazz)))
	 ;; instance fields
	 (let loop ((i (-fx (vector-length fields) 1)))
	    (when (>=fx i 0)
	       (let* ((f (vector-ref-ur fields i))
		      (n (class-field-name f)))
		  (cond
		     ((assq n l)
		      =>
		      (lambda (c)
			 ((class-field-mutator f) inst (cdr c))))
		     ((class-field-default-value? f)
		      ((class-field-mutator f) inst (class-field-default-value f)))
		     ((not (member "nojson" (class-field-info f)))
		      (error "json->ast"
			 (format "Missing field \"~a\"" n) cname)))
		  (loop (-fx i 1)))))
	 ;; constructor
	 (when (procedure? ctor) ctor inst)
	 inst))

   (json-parse ip
      :expr #t
      :undefined #t
      :reviver (lambda (obj key v)
		   (if (and (string? v) (member key '("loc" "endloc")))
		       (let ((i (string-index-right v #\:)))
			  (if i
			      `(at ,(substring v 0 i)
				  ,(string->integer (substring v (+fx i 1))))
			      v))
		       v))
      :array-alloc (lambda ()
		      (make-cell '()))
      :array-set (lambda (a i val)
		    (cell-set! a (cons val (cell-ref a))))
      :array-return (lambda (a i)
		       (reverse! (cell-ref a)))
      :object-alloc (lambda ()
		       (make-cell #f))
      :object-set (lambda (o p val)
		     (cell-set! o
			(cons (cons (string->symbol p) val) (cell-ref o))))
      :object-return (lambda (o)
			(let ((alist (cell-ref o)))
			   (cond
			      ((assq '__node__ alist)
			       =>
			       (lambda (c)
				  (let ((n (cdr c)))
				     (if (not (string? n))
					 (error "json->ast" "Illegal node class" n)
					 (alist->node (string->symbol n) alist)))))
			      ((assq '__ast__ alist)
			       =>
			       (lambda (o) (json-resolve! (cdr o))))
			      ((assq '__symbol__ alist)
			       =>
			       (lambda (a)
				  (string->symbol (cdr a))))
			      ((assq '__ref__ alist)
			       =>
			       (lambda (r)
				  (if (not (integer? (cdr r)))
				      (error "json->ast"
					 "Illegal reference node"
					 o)
				      (instantiate::%JSONDecl
					 (loc '(no-loc))
					 (id 'jsondecl)
					 (%id (cdr r))))))
			      (else
			       (tprint "UNKNWON: " o)
			       o))))
      :parse-error (lambda (msg fname loc)
		      (error "json->ast" "Wrong JSON file" msg))))

;*---------------------------------------------------------------------*/
;*    json-resolve! ...                                                */
;*---------------------------------------------------------------------*/
(define (json-resolve! ast)
   (let* ((vec (make-vector 16))
	  (env (make-cell vec)))
      (json-mark-decl! ast env)
      (json-link-decl! ast (cell-ref env))))

;*---------------------------------------------------------------------*/
;*    json-mark-decl! ::J2SNode ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (json-mark-decl! node::J2SNode env)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    json-mark-decl! ::J2SDecl ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (json-mark-decl! node::J2SDecl env)
   (with-access::J2SDecl node (key)
      (let ((len (vector-length (cell-ref env))))
	 (when (>=fx key len)
	    (cell-set! env (copy-vector (cell-ref env) (+fx key 16))))
	 (vector-set! (cell-ref env) key node)))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    json-mark-decl! ::J2SFun ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (json-mark-decl! node::J2SFun env)
   (with-access::J2SFun node (decl)
      (when decl (json-mark-decl! decl env))
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    json-link-decl! ::J2SNode ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (json-link-decl! node::J2SNode env)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    json-link-decl! ::J2SRef ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (json-link-decl! node::J2SRef env)
   (with-access::J2SRef node (decl)
      (with-access::%JSONDecl decl (%id)
	 (if (or (<fx %id 0) (>fx %id (vector-length env)))
	     (error "json->ast" "Illegal reference" %id)
	     (set! decl (vector-ref env %id)))
	 (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    json-link-decl! ::J2SConsumer ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (json-link-decl! node::J2SDConsumer env)
   (with-access::J2SDConsumer node (decl)
      (with-access::%JSONDecl decl (%id)
	 (if (or (<fx %id 0) (>fx %id (vector-length env)))
	     (error "json->ast" "Illegal reference" %id)
	     (set! decl (vector-ref env %id)))
	 (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    json-link-decl! ::J2SProducer ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (json-link-decl! node::J2SDProducer env)
   (with-access::J2SDProducer node (decl)
      (with-access::%JSONDecl decl (%id)
	 (if (or (<fx %id 0) (>fx %id (vector-length env)))
	     (error "json->ast" "Illegal reference" %id)
	     (set! decl (vector-ref env %id)))
	 (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    json-link-decl! ::J2SPragma ...                                  */
;*---------------------------------------------------------------------*/
(define-method (json-link-decl! node::J2SPragma env)
   (with-access::J2SPragma node (expr)
      (set! expr (call-with-input-string expr read))
      node))

;*---------------------------------------------------------------------*/
;*    j2sdeclinit-val-fun ...                                          */
;*---------------------------------------------------------------------*/
(define (j2sdeclinit-val-fun node::J2SDeclInit)
   (with-access::J2SDeclInit node (val)
      (if (isa? val J2SMethod)
	  (with-access::J2SMethod val (function)
	     function)
	  val)))
