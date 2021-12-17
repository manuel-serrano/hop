;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/ast.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 11 08:54:57 2013                          */
;*    Last change :  Fri Dec 17 09:27:08 2021 (serrano)                */
;*    Copyright   :  2013-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JavaScript AST                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_ast
   
   (library web)
   
   (include "walk.sch"
	    "ast.sch")

   (import  __js2scheme_usage)
   
   (export (abstract-class J2SNode
	      (loc::pair read-only (info '("notraverse")))
	      (%info (default #unspecified) (info '("nojson" "notraverse")))
	      (%%dump (default #unspecified) (info '("nojson" "notraverse")))
	      (%%wstamp (default -1) (info '("nojson" "notraverse"))))
	   
	   (abstract-class J2SStmt::J2SNode)

	   (class J2SMeta::J2SStmt
	      (meta::obj (default #unspecified))
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
	      (pcache-size::long (default 0))
	      (rxcache-size::long (default 0))
	      (call-size::long (default 0))
	      (name read-only (default #f))
	      (main read-only (default #f))
	      (module read-only (default #f))
	      (cnsts::pair-nil (default '()))
	      (decls::pair-nil (default '()) (info '("ast")))
	      (headers::pair-nil (default '()) (info '("ast")))
	      (globals::pair-nil (default '()) (info '("ast")))
	      (strings::pair-nil (default '()) (info '("nojson" "notraverse")))
	      (direct-eval::bool (default #t))
	      (source-map (default #f))
	      (imports::pair-nil (default '()))
	      (exports::pair-nil (default '()) (info '("notraverse"))))

	   (class J2SDecl::J2SStmt
	      id::symbol
	      (_scmid (default #f) (info '("notraverse")))
	      (key (default (ast-decl-key)) (info '("notraverse")))
	      ;; writable=#f iff decl is const
	      (writable (default #t) (info '("notraverse")))
	      ;; either: global, %scope, tls, local, letblock, letvar,
	      ;;   kont, export
	      (scope::symbol (default 'local) (info '("notraverse")))
	      (usecnt::int (default 0) (info '("notraverse")))
	      (useinloop::bool (default #f) (info '("notraverse")))
	      ;; #t (after use) if the variable is captured by a closure
	      (escape::bool (default #f) (info '("notraverse")))
	      ;; see usage-bit.sch
	      (usage::uint32 (default (usage '())))
	      ;; variable range (var, let, let-opt, let-forin, param, class,
	      ;; record, export)
	      (binder::symbol (default 'var) (info '("notraverse")))
	      ;; user declared type (only a mere annotation)
	      (utype (default 'unknown) (info '("notraverse")))
	      ;; initial parameter type
	      (itype (default 'unknown) (info '("notraverse")))
	      ;; computed variable type value
	      (vtype (default 'unknown) (info '("notraverse")))
	      ;; the maximum type of the variable (for classes)
	      (mtype (default 'unknown) (info '("notraverse")))
	      ;; initial parameter range
	      (irange::obj (default #unspecified) (info '("notraverse")))
	      ;; computed variable range
	      (vrange::obj (default #unspecified) (info '("notraverse")))
	      ;; variable 
	      (hint::pair-nil (default '()) (info '("notraverse")))
	      ;; export clause (if any)
	      (export::obj (default #f) (info '("notraverse"))))

	   (class J2SDeclRest::J2SDecl
	      (alloc-policy::symbol (default 'heap) (info '("notraverse"))))
	   
	   (class J2SDeclArguments::J2SDeclRest
	      (argid::symbol read-only (info '("notraverse")))
	      (mode::symbol read-only (info '("notraverse"))))
	   
	   (class J2SDeclInit::J2SDecl
	      (val::J2SExpr (info '("ast"))))

	   (class J2SDeclFun::J2SDeclInit
	      (parent read-only (default #f) (info '("nojson" "notraverse")))
	      (expression::bool (default #f))
	      (hintinfo::obj (default #f) (info '("nojson" "notraverse"))))

	   (class J2SDeclFunType::J2SDeclFun)

	   (class J2SDeclSvc::J2SDeclFun)

	   (class J2SDeclClass::J2SDeclInit)

	   (final-class J2SDeclExtern::J2SDeclInit
	      (bind::bool read-only (default #f))
	      (hidden-class::bool read-only (default #t))
	      ;; #t if an error should be raised on write (for strict mode)
	      (raise-on-write::bool read-only (default #f))
	      ;; extern are sweepable (i.e., removable) when not used
	      ;; by the code generator to optimize expressions
	      ;; the value is a symbol a tell when it can be sweeped
	      ;; its value are: never, scheme, always
	      (sweepable::symbol read-only (default 'scheme))
	      (configurable::bool read-only (default #t)))

	   (final-class J2SDeclImport::J2SDecl
	      (alias read-only (default #f) (info '("notraverse")))
	      (import::obj read-only (info '("notraverse"))))

	   (abstract-class J2SExpr::J2SNode
	      ;; the type of the expression
	      (type (default 'unknown) (info '("notraverse")))
	      ;; the possible types of that expression
	      (hint::pair-nil (default '()) (info '("notraverse")))
	      (range::obj (default #unspecified) (info '("notraverse"))))

	   (class J2SCast::J2SExpr
	      (static::bool (default #f))
	      (expr::J2SExpr (info '("ast"))))
	   
	   (class J2SCheck::J2SExpr
	      (expr::J2SExpr (info '("ast"))))
	   
	   (final-class J2SStmtExpr::J2SStmt
	      (expr::J2SExpr (info '("ast"))))
	   
	   (class J2SIf::J2SStmt
	      (test::J2SExpr (info '("ast")))
	      (then::J2SStmt (info '("ast")))
	      (else::J2SStmt (info '("ast"))))

	   (final-class J2SPrecache::J2SIf
	      (accesses::pair-nil read-only (default '())))
	   
	   (final-class J2SIfIsRecord::J2SIf)
	   
	   (final-class J2SVarDecls::J2SStmt
	      (decls::pair (info '("ast"))))

	   (final-class J2SLetBlock::J2SBlock
	      (rec::bool (default #t))
	      (decls::pair-nil (info '("ast")))
	      (mode::symbol read-only (default 'normal)))
	   
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
	      (utype (default 'unknown) (info '("notraverse")))
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
	      (rtype (default 'unknown) (info '("notraverse")))
	      (rutype (default 'unknown) (info '("notraverse")))
	      (rrange::obj (default #unspecified) (info '("notraverse")))
	      (idthis::obj (default 'this) (info '("notraverse")))
	      (idgen read-only (default #f) (info '("notraverse")))
	      (mode::symbol (default 'normal) (info '("notraverse")))
	      (decl (default #f) (info '("jsonref" "notraverse")))
	      (need-bind-exit-return::bool (default #f) (info '("notraverse")))
	      ;; new-target: unknown | no | global | argument
	      (new-target::symbol (default 'unknown) (info '("notraverse")))
	      ;; #f | rest | arguments
	      (vararg::obj (default #f) (info '("notraverse")))
	      (name::symbol (info '("notraverse")))
	      (generator::bool (default #f) (info '("notraverse")))
	      (optimize (default #t) (info '("notraverse")))
	      (thisp (default #f) (info '("notraverse")))
	      (argumentsp (default #f) (info '("notraverse")))
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
	      (decl (default #f) (info '("jsonref" "notraverse")))
	      (super::J2SExpr (info '("ast")))
	      (src::bool (default #t) (info '("notraverse")))
	      (elements::pair-nil (info '("ast")))
	      (constrsize::int (default 0) (info '("notraverse")))
	      (cmap (default #f))
	      (need-super-check::bool (default #f) (info '("notraverse")))
	      (need-dead-zone-check::bool (default #f) (info '("notraverse")))
	      (methods::obj (default #unspecified) (info '("notraverse"))))

	   (class J2SRecord::J2SClass)

	   (class J2SClassElement::J2SNode
	      (static::bool read-only)
	      (prop::J2SPropertyInit (info '("ast")))
	      (type (default 'any))
	      (clazz (default #f) (info '("notraverse")))
	      (index::long (default -1) (info '("notraverse")))
	      ;; see usage-bit.sch
	      (usage::uint32 (default (usage '()))))
	   
	   (final-class J2SCatch::J2SStmt
	      param::J2SDecl
	      (body::J2SNode (info '("ast"))))
	   
	   (final-class J2STry::J2SStmt
	      (body::J2SBlock (info '("ast")))
	      (catch::J2SStmt (info '("ast")))
	      (finally::J2SStmt (info '("ast"))))
	   
	   (class J2SPragma::J2SExpr
	      (lang::symbol (default 'scheme))
	      (vars::pair-nil (default '()))
	      (vals::pair-nil (default '()) (info '("ast")))
	      (expr (info '("notraverse"))))
	   
	   (class J2SSequence::J2SExpr
	      (exprs::pair (info '("ast"))))
	   
	   (class J2SUnresolvedRef::J2SExpr
	      (cache (default #f))
	      id::symbol)
	   
	   (class J2SGlobalRef::J2SUnresolvedRef
	      (decl::J2SDecl (info '("jsonref"))))
	   
	   (class J2SRef::J2SExpr
	      ;; the declaration
	      (decl::J2SDecl (info '("jsonref" "notraverse"))))
	   
	   (class J2SWithRef::J2SExpr
	      (id::symbol read-only)
	      withs::pair
	      (expr::J2SExpr (info '("ast"))))

	   (class J2SHopRef::J2SExpr
	      (id::symbol read-only)
	      (rtype (default 'any))
	      (module read-only (default #f)))

	   (class J2SLetRef::J2SRef)
	   
	   (final-class J2SAref::J2SRef
	      (array::J2SDecl read-only)
	      (alen::J2SDecl read-only)
	      (amark::obj read-only)
	      (deps::pair-nil read-only))

	   (final-class J2SKontRef::J2SExpr
	      (gen::obj read-only)
	      (index::int read-only)
	      (id::symbol read-only))
	   
	   (final-class J2SThis::J2SRef)
	   
	   (final-class J2SSuper::J2SRef
	      (context (default 'plain) (info '("notraverse"))))

	   (final-class J2SCond::J2SExpr
	      (test::J2SExpr (info '("ast")))
	      (then::J2SExpr (info '("ast")))
	      (else::J2SExpr (info '("ast"))))

	   (abstract-class J2SLiteral::J2SExpr)

	   (final-class J2SArrayAbsent::J2SLiteral)
	   
	   (final-class J2SNull::J2SLiteral)
	   (final-class J2SUndefined::J2SLiteral)

	   (class J2SLiteralValue::J2SLiteral
	      val)

	   (class J2SNativeString::J2SLiteralValue)
	   
	   (class J2SString::J2SLiteralValue
	      (escape::pair-nil read-only (default '()))
	      ;; either #f, #t (after parsing), a J2SClassElement (after symbol)
	      (private (default #f) (info '("notraverse"))))
	   (final-class J2SBool::J2SLiteralValue)
	   (class J2SNumber::J2SLiteralValue)
	   (final-class J2SOctalNumber::J2SNumber)
	   (final-class J2SRegExp::J2SLiteralValue
	      (flags::bstring read-only)
	      (inline::bool (default #f)))
	   (final-class J2SCmap::J2SLiteralValue)

	   (final-class J2SLiteralCnst::J2SLiteral
	      (index::long read-only)
	      (val::J2SExpr read-only (info '("notraverse"))))
	       
	   (final-class J2SArray::J2SLiteral
	      len::int
	      (exprs::pair-nil (info '("ast"))))

	   (final-class J2SSpread::J2SExpr
	      ;; static type of the spread
	      (stype read-only (info '("notraverse")))
	      expr::J2SExpr)
	   
	   (final-class J2STemplate::J2SExpr
	      (exprs::pair (info '("ast"))))
	   
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
	   
;* 	   (final-class J2SVAssig::J2SAssig)                           */
;* 	   (final-class J2SCAssig::J2SAssig)                           */

	   (final-class J2SFunBinding::J2SInit)
	   
	   (final-class J2SPrefix::J2SAssig
	      (cache (default #f) (info '("nojson" "notraverse")))
	      op::symbol)
	   (final-class J2SPostfix::J2SAssig
	      (cache (default #f) (info '("nojson" "notraverse")))
	      op::symbol)
	   (final-class J2SAssigOp::J2SAssig
	      (cache (default #f) (info '("nojson" "notraverse")))
	      op::symbol)
	   
	   (final-class J2SObjInit::J2SExpr
	      (inits::pair-nil (info '("ast")))
	      (cmap (default #f))
	      (ronly (default #f) (info '("notraverse"))))
	   
	   (final-class J2SAccess::J2SExpr
	      (cache (default #f) (info '("nojson" "notraverse")))
	      (cspecs (default '()) (info '("nojson" "notraverse")))
	      (obj::J2SExpr (info '("ast")))
	      (field::J2SExpr (info '("ast"))))

	   (final-class J2SCacheCheck::J2SExpr
	      ;; proto-method | instanceof | method | cmap-proto-method
	      prop::symbol
	      (cache read-only (info '("notraverse")))
	      (owner::obj read-only (default #f) (info '("notraverse")))
	      (obj::J2SExpr (info '("ast")))
	      fields::pair-nil)

	   (final-class J2SCacheUpdate::J2SExpr
	      (prop::symbol read-only)
	      (cache read-only (info '("notraverse")))
	      (obj::J2SExpr (info '("ast"))))

	   (final-class J2SCall::J2SExpr
	      (profid::long (default -1) (info '("notraverse")))
	      (cache (default #f) (info '("nojson" "notraverse")))
	      (cspecs (default '()) (info '("nojson" "notraverse")))
	      (fun::J2SExpr (info '("ast")))
	      (protocol::symbol (default 'direct) (info '("notraverse")))
	      (thisargs::pair-nil (info '("ast")))
	      (args::pair-nil (default '()) (info '("ast"))))
	   
	   (final-class J2STilde::J2SExpr
	      stmt::J2SStmt)
	   
	   (final-class J2SDollar::J2SExpr
	      (node::J2SNode (info '("ast"))))
	   
	   (final-class J2SNew::J2SExpr
	      (caches (default '()))
	      (clazz::J2SNode (info '("ast")))
	      (protocol::symbol (default 'direct) (info '("notraverse")))
	      (args::pair-nil (info '("ast"))))

	   (class J2SPropertyInit::J2SNode
	      (name::J2SExpr (info '("ast")))
	      (cache (default #f) (info '("nojson" "notraverse"))))
	   
	   (class J2SDataPropertyInit::J2SPropertyInit
	      (val::J2SExpr (info '("ast"))))
	   
	   (final-class J2SMethodPropertyInit::J2SDataPropertyInit
	      (inlinecachevar (default #f)))
	   
	   (final-class J2SAccessorPropertyInit::J2SPropertyInit
	      (get::obj (default #f) (info '("ast")))
	      (set::obj (default #f) (info '("ast"))))

	   (final-class J2SKont::J2SExpr
	      (param::J2SDecl read-only)
	      (exn::J2SDecl read-only)
	      (body::J2SNode (info '("ast"))))

	   (final-class J2SOPTInitSeq::J2SSeq
	      ref::J2SRef
	      (cmap read-only)
	      (cache (default #f))
	      (offset::symbol read-only)
	      (cnt::symbol read-only))

	   (final-class J2SDProducer::J2SExpr
	      (decl::J2SDecl (info '("jsonref")))
	      (expr::J2SExpr (info '("ast")))
	      (size::long read-only (info '("notraverse"))))

	   (final-class J2SDConsumer::J2SExpr
	      (decl::J2SDecl (info '("jsonref")))
	      (expr::J2SExpr (info '("ast")))
	      (path read-only))

	   (final-class J2SExport
	      (loc read-only)
	      (id::symbol read-only)
	      (alias::symbol read-only)
	      (index::long (default -1))
	      (decl (default #f) (info '("jsonref")))
	      (from (default #f)))

	   (final-class J2SImportPath
	      (loc read-only)
	      (name::bstring read-only)
	      (path::bstring read-only)
	      (protocol::symbol read-only)
	      (index::long (default -1))
	      (checksum::long (default -1)))

	   (final-class J2SImport::J2SStmt
	      (path::bstring read-only (info '("notraverse")))
	      (respath (default #f) (info '("notraverse")))
	      ;; dollarpath is only used for client-side imports whose
	      ;; module name is a dollar expression (see js.scm)
	      dollarpath::J2SExpr
	      (names::pair-nil (default '()) (info '("notraverse")))
	      (mvar (default #f) (info '("notraverse")))
	      (ivar (default #f) (info '("notraverse")))
	      (reindex::long (default -1) (info '("notraverse")))
	      (iprgm (default #f) (info '("notraverse")))
	      (lang (default #f)))

	   (final-class J2SImportName
	      (loc read-only)
	      (id::symbol read-only)
	      (alias::symbol read-only))

	   (final-class J2SImportNamespace
	      (loc read-only)
	      (id::symbol read-only))

	   (final-class J2SImportRedirect
	      (loc read-only)
	      (id::symbol read-only)
	      (alias::symbol read-only))

	   (final-class J2SImportExport
	      (loc read-only))

	   (final-class J2SImportDynamic::J2SExpr
	      (base::bstring (default (pwd)))
	      path::J2SExpr)

	   (final-class J2SImportExports::J2SExpr
	      import)

	   (final-class J2SExportVars::J2SStmt
	      (refs::pair-nil read-only)
	      (aliases::pair-nil read-only (info '("notraverse")))
	      (program (default #f) (info '("notraverse"))))

	   (generic walk0 n::J2SNode p::procedure)
	   (generic walk1 n::J2SNode p::procedure a0)
	   (generic walk2 n::J2SNode p::procedure a0 a1)
	   (generic walk3 n::J2SNode p::procedure a0 a1 a2)
	   (generic walk4 n::J2SNode p::procedure a0 a1 a2 a3)
	   (generic walk5 n::J2SNode p::procedure a0 a1 a2 a3 a4)
	   (generic walk6 n::J2SNode p::procedure a0 a1 a2 a3 a4 a5)
	   (generic walk7 n::J2SNode p::procedure a0 a1 a2 a3 a4 a5 a6)
	   (generic walk8 n::J2SNode p::procedure a0 a1 a2 a3 a4 a5 a6 a7)
	   
	   (generic walk0*::pair-nil n::J2SNode p::procedure)
	   (generic walk1*::pair-nil n::J2SNode p::procedure a0)
	   (generic walk2*::pair-nil n::J2SNode p::procedure a0 a1)
	   (generic walk3*::pair-nil n::J2SNode p::procedure a0 a1 a2)
	   (generic walk4*::pair-nil n::J2SNode p::procedure a0 a1 a2 a3)
	   (generic walk5*::pair-nil n::J2SNode p::procedure a0 a1 a2 a3 a4)
	   (generic walk6*::pair-nil n::J2SNode p::procedure a0 a1 a2 a3 a4 a5)
	   (generic walk7*::pair-nil n::J2SNode p::procedure a0 a1 a2 a3 a4 a5 a6)
	   (generic walk8*::pair-nil n::J2SNode p::procedure a0 a1 a2 a3 a4 a5 a6 a7)
	   
	   (generic walk0!::J2SNode n::J2SNode p::procedure)
	   (generic walk1!::J2SNode n::J2SNode p::procedure a0)
	   (generic walk2!::J2SNode n::J2SNode p::procedure a0 a1)
	   (generic walk3!::J2SNode n::J2SNode p::procedure a0 a1 a2)
	   (generic walk4!::J2SNode n::J2SNode p::procedure a0 a1 a2 a3)
	   (generic walk5!::J2SNode n::J2SNode p::procedure a0 a1 a2 a3 a4)
	   (generic walk6!::J2SNode n::J2SNode p::procedure a0 a1 a2 a3 a4 a5)
	   (generic walk7!::J2SNode n::J2SNode p::procedure a0 a1 a2 a3 a4 a5 a6)
	   (generic walk8!::J2SNode n::J2SNode p::procedure a0 a1 a2 a3 a4 a5 a6 a7)
	   
	   (macro define-walk-method)

	   (node-%info ::J2SNode)
	   (node-loc ::J2SNode)
	   (node-endloc ::J2SNode)
	   
	   (j2sfun-expression? ::J2SFun)
	   (j2s-chaining? ::J2SExpr)
	   
	   (ast-decl-key::int)
	   
	   (ast->json ::obj ::output-port)
	   (json->ast ::input-port)

	   (nodefval::J2SUndefined)
	   (nodefval?::bool ::J2SExpr)

	   (j2s-var?::bool ::J2SDecl)
	   (j2s-let?::bool ::J2SDecl)
	   (j2s-const?::bool ::J2SDecl)
	   (j2s-param?::bool ::J2SDecl)
	   (j2s-export?::bool ::J2SDecl)
	   (j2s-global?::bool ::J2SDecl)
	   (j2s-let-opt?::bool ::J2SDecl)
	   (j2s-let-class?::bool ::J2SDecl)
	   (j2s-new-target?::bool ::J2SNode)
	   (j2s-decl-class?::bool ::J2SDecl)
	   (j2s-decl-record?::bool ::J2SDecl)

	   (j2s-field-name::obj ::J2SNode)
	   (inline j2s-field-length?::bool ::J2SNode)

	   (j2sdeclinit-val-fun::J2SExpr ::J2SDeclInit)

	   (j2sprogram-get-export-index::long ::J2SProgram))
   
   (static (class %JSONDecl::J2SDecl
	      (%id read-only))))

;*---------------------------------------------------------------------*/
;*    node-%info ...                                                   */
;*---------------------------------------------------------------------*/
(define (node-%info n)
   (with-access::J2SNode n (%info) %info))

;*---------------------------------------------------------------------*/
;*    node-loc ...                                                     */
;*---------------------------------------------------------------------*/
(define (node-loc n)
   (with-access::J2SNode n (loc) loc))

;*---------------------------------------------------------------------*/
;*    node-endloc ...                                                  */
;*---------------------------------------------------------------------*/
(define (node-endloc n)
   (if (isa? n J2SBlock)
       (with-access::J2SBlock n (endloc) endloc)
       (with-access::J2SNode n (loc) loc)))

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
	 ((let let-opt let-forin) #t)
	 ((param class record export) #f)
	 (else (error "j2s-var?" "wrong binder" (vector loc id binder))))))

;*---------------------------------------------------------------------*/
;*    j2s-let? ...                                                     */
;*---------------------------------------------------------------------*/
(define (j2s-let? decl::J2SDecl)
   (with-access::J2SDecl decl (binder id loc)
      (case binder
	 ((let let-opt let-forin) #t)
	 ((var param class record export) #f)
	 (else (error "j2s-let?" "wrong binder" (vector loc id binder))))))

;*---------------------------------------------------------------------*/
;*    j2s-const? ...                                                   */
;*---------------------------------------------------------------------*/
(define (j2s-const? decl::J2SDecl)
   (with-access::J2SDecl decl (binder writable id loc)
      (unless writable
	 (case binder
	    ((let let-opt let-forin export) #t)
	    ((var param class record) #f)
	    (else (error "j2s-const?" "wrong binder" (vector loc id binder)))))))

;*---------------------------------------------------------------------*/
;*    j2s-let-opt? ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-let-opt? decl::J2SDecl)
   (with-access::J2SDecl decl (binder id loc)
      (case binder
	 ((let-opt let-forin) #t)
	 ((let var param class record export) #f)
	 (else (error "j2s-let-opt?" "wrong binder" (vector loc id binder))))))

;*---------------------------------------------------------------------*/
;*    j2s-let-class? ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-let-class? this)
   (when (and (j2s-let? this) (isa? this J2SDeclClass))
      (with-access::J2SDeclClass this (val)
	 (isa? val J2SClass))))

;*---------------------------------------------------------------------*/
;*    j2s-new-target? ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-new-target? this::J2SNode)
   (when (isa? this J2SPragma)
      (with-access::J2SPragma this (lang expr)
	 (and (eq? lang 'javascript) (equal? expr "new.target")))))

;*---------------------------------------------------------------------*/
;*    j2s-decl-class? ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-decl-class? this::J2SDecl)
   (when (isa? this J2SDeclClass)
      (with-access::J2SDeclInit this (val)
	 (when (isa? val J2SClass)
	    (with-access::J2SClass val (need-dead-zone-check)
	       (not need-dead-zone-check))))))

;*---------------------------------------------------------------------*/
;*    j2s-decl-record? ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-decl-record? this::J2SDecl)
   (when (isa? this J2SDeclInit)
      (with-access::J2SDeclInit this (val)
	 (isa? val J2SRecord))))

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
	 ((var let const let-opt let-forin const-opt class record export) #f)
	 (else (error "j2s-param?" "wrong binder" (vector loc id binder))))))

;*---------------------------------------------------------------------*/
;*    j2s-export? ...                                                  */
;*---------------------------------------------------------------------*/
(define (j2s-export? decl::J2SDecl)
   (with-access::J2SDecl decl (binder scope)
      (or (eq? binder 'export) (eq? scope 'export))))

;*---------------------------------------------------------------------*/
;*    j2s-global? ...                                                  */
;*---------------------------------------------------------------------*/
(define (j2s-global? decl::J2SDecl)
   (with-access::J2SDecl decl (scope)
      (memq scope '(global export %scope))))

;*---------------------------------------------------------------------*/
;*    j2sfun-expression? ...                                           */
;*---------------------------------------------------------------------*/
(define (j2sfun-expression? this::J2SFun)
   (with-access::J2SFun this (decl)
      (when (isa? decl J2SDeclFun)
	 (with-access::J2SDeclFun decl (expression) expression))))

;*---------------------------------------------------------------------*/
;*    j2s-chaining? ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-chaining? this::J2SExpr)
   (when (isa? this J2SUnary)
      (with-access::J2SUnary this (op)
	 (eq? op '?.))))
				      
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
   (error "walk0" (format "Illegal node type \"~a\"" (typeof n))
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk1 n::J2SNode p::procedure arg0)
   (error "walk1" (format "Illegal node type \"~a\"" (typeof n))
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk2 n::J2SNode p::procedure arg0 arg1)
   (error "walk2" (format "Illegal node type \"~a\"" (typeof n))
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk3 n::J2SNode p::procedure arg0 arg1 arg2)
   (error "walk3" (format "Illegal node type \"~a\"" (typeof n))
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk4 n::J2SNode p::procedure arg0 arg1 arg2 arg3)
   (error "walk4" (format "Illegal node type \"~a\"" (typeof n))
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk5 n::J2SNode p::procedure arg0 arg1 arg2 arg3 arg4)
   (error "walk5" (format "Illegal node type \"~a\"" (typeof n))
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk6 n::J2SNode p::procedure arg0 arg1 arg2 arg3 arg4 arg5)
   (error "walk6" (format "Illegal node type \"~a\"" (typeof n))
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk7 n::J2SNode p::procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6)
   (error "walk7" (format "Illegal node type \"~a\"" (typeof n))
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk8 n::J2SNode p::procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 a7)
   (error "walk8" (format "Illegal node type \"~a\"" (typeof n))
      (with-output-to-string (lambda () (write-circle n)))))

(define-generic (walk0*::pair-nil n::J2SNode p::procedure)
   (error "walk0*" (format "Illegal node type \"~a\"" (typeof n))
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk1*::pair-nil n::J2SNode p::procedure arg0)
   (error "walk1*" (format "Illegal node type \"~a\"" (typeof n))
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk2*::pair-nil n::J2SNode p::procedure arg0 arg1)
   (error "walk2*" (format "Illegal node type \"~a\"" (typeof n))
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk3*::pair-nil n::J2SNode p::procedure arg0 arg1 arg2)
   (error "walk3*" (format "Illegal node type \"~a\"" (typeof n))
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk4*::pair-nil n::J2SNode p::procedure arg0 arg1 arg2 arg3)
   (error "walk4!" (format "Illegal node type \"~a\"" (typeof n))
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk5*::pair-nil n::J2SNode p::procedure arg0 arg1 arg2 arg3 arg4)
   (error "walk5*" (format "Illegal node type \"~a\"" (typeof n))
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk6*::pair-nil n::J2SNode p::procedure arg0 arg1 arg2 arg3 arg4 arg5)
   (error "walk6*" (format "Illegal node type \"~a\"" (typeof n))
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk7*::pair-nil n::J2SNode p::procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6)
   (error "walk7*" (format "Illegal node type \"~a\"" (typeof n))
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk8*::pair-nil n::J2SNode p::procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7)
   (error "walk8*" (format "Illegal node type \"~a\"" (typeof n))
      (with-output-to-string (lambda () (write-circle n)))))

(define-generic (walk0!::J2SNode n::J2SNode p::procedure)
   (error "walk0!" (format "Illegal node type \"~a\"" (typeof n))
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk1!::J2SNode n::J2SNode p::procedure arg0)
   (error "walk1!" (format "Illegal node type \"~a\"" (typeof n))
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk2!::J2SNode n::J2SNode p::procedure arg0 arg1)
   (error "walk2!" (format "Illegal node type \"~a\"" (typeof n))
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk3!::J2SNode n::J2SNode p::procedure arg0 arg1 arg2)
   (error "walk3!" (format "Illegal node type \"~a\"" (typeof n))
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk4!::J2SNode n::J2SNode p::procedure arg0 arg1 arg2 arg3)
   (error "walk4!" (format "Illegal node type \"~a\"" (typeof n))
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk5!::J2SNode n::J2SNode p::procedure arg0 arg1 arg2 arg3 arg4)
   (error "walk5!" (format "Illegal node type \"~a\"" (typeof n))
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk6!::J2SNode n::J2SNode p::procedure arg0 arg1 arg2 arg3 arg4 arg5)
   (error "walk6!" (format "Illegal node type \"~a\"" (typeof n))
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk7!::J2SNode n::J2SNode p::procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6)
   (error "walk7!" (format "Illegal node type \"~a\"" (typeof n))
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk8!::J2SNode n::J2SNode p::procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7)
   (error "walk8!" (format "Illegal node type \"~a\"" (typeof n))
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
       ,@(map (lambda (nb) (gen-method nb)) (iota 9))
       ,@(map (lambda (nb) (gen-method* nb)) (iota 9))
       ,@(map (lambda (nb) (gen-method! nb)) (iota 9))))

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
(gen-walks J2SProgram (headers) (decls) (nodes))
(gen-walks J2SBindExit stmt)
(gen-walks J2SReturn expr)
(gen-walks J2SReturnYield expr kont)
(gen-walks J2SWith obj block)
(gen-walks J2SThrow expr)
(gen-walks J2STry body catch finally)
(gen-walks J2SCatch body)
(gen-walks J2SStmtExpr expr)
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
(gen-walks J2SAccess obj field)
(gen-walks J2SCacheCheck obj)
(gen-walks J2SCacheUpdate obj)
(gen-walks J2SCall fun (thisargs) (args))
(gen-walks J2SNew clazz (args))
(gen-walks J2SAssig lhs rhs)
(gen-walks J2SFun body (params))
(gen-walks J2SSvc body init (params))
(gen-walks J2SMethod function method)
(gen-walks J2SObjInit (inits))
(gen-walks J2SDataPropertyInit name val)
(gen-walks J2SAccessorPropertyInit name get set)
(gen-walks J2SArray (exprs))
(gen-walks J2SSpread expr)
(gen-walks J2SDeclInit val)
(gen-walks J2SWithRef expr)
(gen-walks J2SIf test then else)
(gen-walks J2SPrecache (accesses) test then else)
(gen-walks J2SIfIsRecord test then else)
(gen-walks J2SCond test then else)
(gen-walks J2SDollar node)
(gen-walks J2SYield expr)
(gen-walks J2SKont body)
(gen-walks J2SCast expr)
(gen-walks J2SCheck expr)
(gen-walks J2SClass super (elements))
(gen-walks J2SClassElement prop)
(gen-walks J2SDProducer expr)
(gen-walks J2SDConsumer expr)
(gen-walks J2SPragma (vals))
(gen-walks J2SImport dollarpath)
(gen-walks J2SImportDynamic path)
(gen-walks J2SExportVars (refs))

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
       (display "null" op))
      (#t
       (display "true" op))
      (#f
       (display "false" op))
      ((? interval? )
       (interval->json this op))
      ((? vector?)
       (display "[" op)
       (let loop ((i 0)
		  (sep ""))
	  (if (<fx i (vector-length this))
	      (begin
		 (display sep op)
		 (j2s->json (vector-ref this i) op)
		 (loop (+fx i 1) ", "))
	      (display "]" op))))
      (else
       ;; other literal
       (display this op))))

;*---------------------------------------------------------------------*/
;*    interval->json ...                                               */
;*---------------------------------------------------------------------*/
(define (interval->json this op::output-port)
   (let ((min (interval-min this))
	 (max (interval-max this)))
      (cond
	 ((and (<=llong min *-inf.0*) (>= max *+inf.0*))
	  (display "null" op))
	 ((<=llong min *-inf.0*)
	  (fprintf op "{ \"max\": ~a }" max))
	 ((>= max *+inf.0*)
	  (fprintf op "{ \"min\": ~a }" min))
	 (else
	  (fprintf op "{ \"min\": ~a, \"max\": ~a }" min max)))))

(define *+inf.0* (exptllong #l2 54))
(define *-inf.0* (negllong (exptllong #l2 54)))

;*---------------------------------------------------------------------*/
;*    j2s-decl->json ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-decl->json this::J2SDecl clazz loc op::output-port)
   (with-access::J2SDecl this (key)
      (fprintf op "{\"__ref__\": ~a, " key)
      (fprintf op "\"__nodeType__\": \"~a\"" clazz)
      (when loc
	 (display ", \"loc\": " op)
	 (j2s->json loc op))
      (display "}" op)))

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
		     (let ((v ((class-field-accessor f) this)))
			(fprintf op ", \"~a\": " (class-field-name f))
			(cond
			   ((and (pair? fi) (member "jsonref" fi)
				 (isa? v J2SDecl))
			    (with-access::J2SNode this (loc)
			       (j2s-decl->json v (class-name clazz) loc op)))
			   (else
			    (j2s->json v op))))))
	       (for (-fx i 1)))))
      (display " }" op)))

;*---------------------------------------------------------------------*/
;*    j2s->json ::J2SPragma ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s->json this::J2SPragma op::output-port)
   (with-access::J2SPragma this (expr loc lang vars vals hint type)
      (display "{ \"__node__\": \"J2SPragma\", \"expr\": \"" op)
      (display (string-for-read
		  (call-with-output-string
		     (lambda (op) (write expr op))))
	 op)
      (display "\", \"vars\": " op)
      (display (format "[~(, )]" (map (lambda (s) (format "~s")) vars)) op)
      (display ", \"vals\": [" op)
      (if (null? vals)
	  (display "]" op)
	  (let loop ((vals vals))
	     (if (null? (cdr vals))
		 (begin
		    (j2s->json (car vals) op)
		    (display "]" op))
		 (begin
		    (j2s->json (car vals) op)
		    (display ", " op)
		    (loop (cdr vals))))))
      (display ", \"lang\": " op)
      (j2s->json lang op)
      (display ", \"hint\": " op)
      (j2s->json hint op)
      (display ", \"type\": " op)
      (j2s->json type op)
      (display ", \"loc\": " op)
      (j2s->json loc op)
      (display " }" op)))

;*---------------------------------------------------------------------*/
;*    j2s->json ::J2SBindExit ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s->json this::J2SBindExit op::output-port)
   (with-access::J2SBindExit this (lbl stmt)
      (display "{ \"__node__\": \"J2SBindExit\"," op)
      (when lbl
	 (display "\"lbl\": \"" op)
	 (display lbl op)
	 (display "\"," op))
      (display "\"stmt\": " op)
      (j2s->json stmt op)
      (display "\"}" op)))

;*---------------------------------------------------------------------*/
;*    j2s->json ::J2SReturn ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s->json this::J2SReturn op::output-port)
   (with-access::J2SReturn this (from expr loc tail exit)
      (display "{ \"__node__\": \"J2SReturn\"," op)
      (display "\"loc\": " op)
      (j2s->json loc op)
      (display ", " op)
      (display "\"exit\": " op)
      (j2s->json exit op)
      (display ", " op)
      (display "\"tail\": " op)
      (j2s->json tail op)
      (display ", " op)
      (when (isa? from J2SBindExit)
	 (with-access::J2SBindExit from (lbl)
	    (display "\"from\": \"" op)
	    (display lbl op)
	    (display "\"," op)))
      (display "\"expr\": " op)
      (j2s->json expr op)
      (display "}" op)))
            
;*---------------------------------------------------------------------*/
;*    j2s->json ::J2SExport ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s->json this::J2SExport op::output-port)
   (with-access::J2SExport this (id alias index decl from)
      (display "{ \"__node__\": \"J2SExport\", \"id\": " op)
      (j2s->json id op)
      (display ", \"index\": " op)
      (display index op)
      (display ",\"alias\": " op)
      (j2s->json alias op)
      (display ",\"decl\": " op)
      (if decl
	  (j2s-decl->json decl "J2SExport" #f op)
	  (display "false" op))
      (display ",\"from\": " op)
      (j2s->json from op)
      (display "}" op)))

;*---------------------------------------------------------------------*/
;*    j2s->json ::J2SImportName ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s->json this::J2SImportName op::output-port)
   (with-access::J2SImportName this (loc id alias)
      (display "{ \"__node__\": \"J2SImportName\", \"id\": " op)
      (j2s->json id op)
      (display ", \"alias\": " op)
      (j2s->json alias op)
      (display ", \"loc\": " op)
      (j2s->json loc op)
      (display " }" op)))
      
;*---------------------------------------------------------------------*/
;*    j2s->json ::J2SImportNamespace ...                               */
;*---------------------------------------------------------------------*/
(define-method (j2s->json this::J2SImportNamespace op::output-port)
   (with-access::J2SImportNamespace this (loc id)
      (display "{ \"__node__\": \"J2SImportNamespace\", \"id\": " op)
      (j2s->json id op)
      (display ", \"loc\": " op)
      (j2s->json loc op)
      (display " }" op)))
      
;*---------------------------------------------------------------------*/
;*    j2s->json ::J2SImportRedirect ...                                */
;*---------------------------------------------------------------------*/
(define-method (j2s->json this::J2SImportRedirect op::output-port)
   (with-access::J2SImportRedirect this (loc id alias)
      (display "{ \"__node__\": \"J2SImportRedirect\", \"id\": " op)
      (j2s->json id op)
      (display ", \"alias\": " op)
      (j2s->json alias op)
      (display ", \"loc\": " op)
      (j2s->json loc op)
      (display " }" op)))
      
;*---------------------------------------------------------------------*/
;*    j2s->json ::J2SImportExport ...                                  */
;*---------------------------------------------------------------------*/
(define-method (j2s->json this::J2SImportExport op::output-port)
   (with-access::J2SImportExport this (loc id)
      (display "{ \"__node__\": \"J2SImportExport\", \"loc\": \"" op)
      (j2s->json loc op)
      (display " }" op)))
      
;*---------------------------------------------------------------------*/
;*    json->ast ...                                                    */
;*---------------------------------------------------------------------*/
(define (json->ast ip::input-port)

   (define (cast val ty)
      (case ty
	 ((uint32)
	  (fixnum->uint32 val))
	 ((int32)
	  (fixnum->int32 val))
	 ((symbol)
	  (if (string? val) (string->symbol val) val))
	 (else
	  val)))

   (define (alist->node cname l)
      (let* ((clazz (find-class cname))
	     (ctor (class-constructor clazz))
	     (inst ((class-allocator clazz)))
	     (fields (class-all-fields clazz)))
	 ;; instance fields
	 (let loop ((i (-fx (vector-length fields) 1)))
	    (when (>=fx i 0)
	       (let* ((f (vector-ref-ur fields i))
		      (n (class-field-name f))
		      (t (class-field-type f)))
		  (cond
		     ((assq n l)
		      =>
		      (lambda (c)
			 (with-handler
			    (lambda (exn)
			       (error "json->ast"
				  (format "~a: illegal field assignment \"~s::~a\"" cname n t)
				  (cdr c)))
			    ((class-field-mutator f) inst (cast (cdr c) t)))))
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
      :array-alloc (lambda ()
		      (make-cell '()))
      :array-set (lambda (a i val)
		    (cell-set! a (cons val (cell-ref a))))
      :array-return (lambda (a i)
		       (reverse! (cell-ref a)))
      :object-alloc (lambda ()
		       (make-cell '()))
      :object-set (lambda (o p val)
		     (when (and (member p '("loc" "endloc")) (string? val))
			(let ((i (string-index-right val #\:)))
			   (when i
			      (set! val
				 `(at ,(substring val 0 i)
				     ,(string->integer
					 (substring val (+fx i 1))))))))
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
					 alist)
				      (instantiate::%JSONDecl
					 (loc '(no-loc))
					 (id 'jsondecl)
					 (%id (cdr r))))))
			      ((assq '__undefined__ alist)
			       #unspecified)
			      ((assq '__car__ alist)
			       =>
			       (lambda (a)
				  (cons (cdr a) (cdr (assq '__cdr__ alist)))))
			      (else
			       (tprint "UNKNWON: " (cell-ref o))
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

;*---------------------------------------------------------------------*/
;*    j2sprogram-get-export-index ...                                  */
;*    -------------------------------------------------------------    */
;*    Returns the next available index for export.                     */
;*---------------------------------------------------------------------*/
(define (j2sprogram-get-export-index::long prgm::J2SProgram)
   (with-access::J2SProgram prgm (exports)
      (let loop ((exports exports)
		 (i -1))
	 (if (null? exports)
	     (+fx i 1)
	     (with-access::J2SExport (car exports) (index from id)
		(cond
		   ((isa? from J2SProgram)
		    (loop (cdr exports) i))
		   ((> index i)
		    (loop (cdr exports) index))
		   (else
		    (loop (cdr exports) i))))))))

;*---------------------------------------------------------------------*/
;*    exptllong ...                                                    */
;*---------------------------------------------------------------------*/
(define (exptllong n exp::long)
   (if (=llong n #l2)
       (bit-lshllong #l1 exp)
       (error "exptllong" "wrong number" n)))


