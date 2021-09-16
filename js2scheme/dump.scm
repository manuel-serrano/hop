;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/dump.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 11 11:12:21 2013                          */
;*    Last change :  Thu Sep 16 17:39:24 2021 (serrano)                */
;*    Copyright   :  2013-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Dump the AST for debugging                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_dump

   (include "ast.sch"
	    "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_utils
	   __js2scheme_stage
	   __js2scheme_node-size)

   (export j2s-dump-stage
	   (j2s-dump-decls::obj ::obj)
	   (j2s-dump-range ::obj)
	   (j2s-dump-register-struct-info! ::symbol ::procedure)
	   (type->sexp typ)
	   (generic j2s->list ::obj)
	   (generic j2s-info->list ::obj)
	   (generic j2ssum::long ::obj)))

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
;*    dump-need-bind-exit-return ...                                   */
;*---------------------------------------------------------------------*/
(define (dump-need-bind-exit-return need-bind-exit-return)
   (if (and (or (>= (bigloo-debug) 3)
		(string-contains (or (getenv "HOPTRACE") "") "j2s:need-bind-exit-return")))
       `(:need-bind-exit-return ,need-bind-exit-return)
       '()))

;*---------------------------------------------------------------------*/
;*    dump-new-target ...                                              */
;*---------------------------------------------------------------------*/
(define (dump-new-target new-target)
   (if (eq? new-target 'unknown) '() `(:new-target ,new-target)))

;*---------------------------------------------------------------------*/
;*    dump-size ...                                                    */
;*---------------------------------------------------------------------*/
(define (dump-size this::J2SNode)
   (if (string-contains (or (getenv "HOPTRACE") "") "j2s:size")
       `(:size ,(node-size this))
       '()))

;*---------------------------------------------------------------------*/
;*    dump-tail ...                                                    */
;*---------------------------------------------------------------------*/
(define (dump-tail tail)
   (if (string-contains (or (getenv "HOPTRACE") "") "j2s:tail")
       `(:tail ,tail)
       '()))

;*---------------------------------------------------------------------*/
;*    type->sexp ...                                                   */
;*---------------------------------------------------------------------*/
(define (type->sexp type)
   (cond
      ((symbol? type)
       type)
      ((isa? type J2SRecord)
       (with-access::J2SRecord type (name)
	  (cons 'record name)))
      ((isa? type J2SClass)
       (with-access::J2SClass type (name)
	  (cons 'class name)))
      (else
       type)))

;*---------------------------------------------------------------------*/
;*    j2s-info->list ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (j2s-info->list this)
   (cond
      ((and (pair? this) (not (or (null? (cdr this)) (pair? (cdr this)))))
       (cons (j2s-info->list (car this)) (j2s-info->list (cdr this))))
      ((list? this)
       (map j2s-info->list this))
      ((or (string? this) (symbol? this) (number? this) (boolean? this))
       this)
      ((or (char? this) (eq? this #unspecified))
       this)
      ((struct? this)
       (struct-info->list this))
      ((isa? this J2SString)
       (with-access::J2SString this (val)
	  (format "[[J2SString:~a]]" val)))
      ((isa? this J2SDecl)
       (with-access::J2SDecl this (id)
	  (format "[[J2SDecl:~a]]" id)))
      (else
       (format "[[~a]]" (typeof this)))))

;*---------------------------------------------------------------------*/
;*    struct-info-dumps ...                                            */
;*---------------------------------------------------------------------*/
(define struct-info-dumps '())

;*---------------------------------------------------------------------*/
;*    j2s-dump-register-struct-info! ...                               */
;*---------------------------------------------------------------------*/
(define (j2s-dump-register-struct-info! key proc)
   (set! struct-info-dumps (cons (cons key proc) struct-info-dumps)))

;*---------------------------------------------------------------------*/
;*    struct-info->list ...                                            */
;*---------------------------------------------------------------------*/
(define (struct-info->list this)
   (let ((proc (assq (struct-key this) struct-info-dumps)))
      (if (pair? proc)
	  ((cdr proc) this)
	  (let ((o (make-struct (struct-key this) (struct-length this) #f)))
	     (let loop ((i 0))
		(if (=fx i (struct-length this))
		    o
		    (begin
		       (struct-set! o i (j2s-info->list (struct-ref this i)))
		       (loop (+fx i 1)))))))))

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
	  `(:type ,(type->sexp type))
	  '())))

;*---------------------------------------------------------------------*/
;*    dump-protocol ...                                                */
;*---------------------------------------------------------------------*/
(define (dump-protocol protocol)
   (if (or (>= (bigloo-debug) 2)
	   (string-contains (or (getenv "HOPTRACE") "") "j2s:protocol"))
       `(:protocol ,protocol)
       '()))

;*---------------------------------------------------------------------*/
;*    dump-rtype ...                                                   */
;*---------------------------------------------------------------------*/
(define (dump-rtype this)
   (if (or (>= (bigloo-debug) 2)
	   (string-contains  (or (getenv "HOPTRACE") "") "j2s:type"))
       (let loop ((this this))
	  (cond
	     ((isa? this J2SFun)
	      (with-access::J2SFun this (rtype)
		 `(:rtype ,(type->sexp rtype))))
	     ((isa? this J2SRef)
	      (with-access::J2SRef this (decl)
		 (if (isa? decl J2SDeclInit)
		     (with-access::J2SDeclInit decl (val)
			(loop val))
		     '())))
	     (else
	      '())))
       '()))
      
;*---------------------------------------------------------------------*/
;*    dump-rutype ...                                                  */
;*---------------------------------------------------------------------*/
(define (dump-rutype this)
   (if (or (>= (bigloo-debug) 2)
	   (string-contains  (or (getenv "HOPTRACE") "") "j2s:type+"))
       (with-access::J2SFun this (rutype)
	  `(:rutype ,(type->sexp rutype)))
       '()))
      
;*---------------------------------------------------------------------*/
;*    dump-vtype ...                                                   */
;*---------------------------------------------------------------------*/
(define (dump-vtype this::J2SDecl)
   (with-access::J2SDecl this (vtype utype itype)
      (cond
	 ((or (>= (bigloo-debug) 2)
	      (string-contains (or (getenv "HOPTRACE") "") "j2s:type+")
	      (string-contains (or (getenv "HOPTRACE") "") "j2s:vtype"))
	  (if (isa? this J2SDeclFun)
	      (with-access::J2SDeclFun this (val)
		 (with-access::J2SFun val (rtype)
		    `(:vtype ,(type->sexp vtype)
			:utype ,(type->sexp utype)
			:itype ,(type->sexp itype)
			:rtype ,(type->sexp rtype))))
	      `(:vtype ,(type->sexp vtype)
		  :utype ,(type->sexp utype)
		  :itype ,(type->sexp itype))))
	 ((or (>= (bigloo-debug) 2)
	      (string-contains (or (getenv "HOPTRACE") "") "j2s:type"))
	  `(:vtype ,(type->sexp vtype)))
	 (else
	  '()))))
      
;*---------------------------------------------------------------------*/
;*    dump-scope ...                                                   */
;*---------------------------------------------------------------------*/
(define (dump-scope scope)
   (if (or (>= (bigloo-debug) 3)
	   (string-contains (or (getenv "HOPTRACE") "") "j2s:scope"))
       `(:scope ,scope)
       '()))
      
;*---------------------------------------------------------------------*/
;*    dump-hint ...                                                    */
;*---------------------------------------------------------------------*/
(define (dump-hint this)
   
   (define (dump-hint-value val)
      (if (=fx (cdr val) (minvalfx))
	  (cons (car val) -inf.0)
	  val))
   
   (if (or (>= (bigloo-debug) 2)
	   (string-contains (or (getenv "HOPTRACE") "") "j2s:hint"))
       (cond
	  ((isa? this J2SDecl)
	   (with-access::J2SDecl this (hint)
	      (if (pair? hint) `(:hint ,(map dump-hint-value hint)) '())))
	  ((isa? this J2SExpr)
	   (with-access::J2SExpr this (hint)
	      (if (pair? hint) `(:hint ,(map dump-hint-value hint)) '()))))
       '()))

;*---------------------------------------------------------------------*/
;*    exptllong ...                                                    */
;*---------------------------------------------------------------------*/
(define (exptllong n exp::long)
   (if (=llong n #l2)
       (bit-lshllong #l1 exp)
       (error "exptllong" "wrong number" n)))

;*---------------------------------------------------------------------*/
;*    integer bounds                                                   */
;*---------------------------------------------------------------------*/
(define *max-length* (-llong (exptllong #l2 32) #l1))
(define *max-index* (-llong *max-length* #l1))
(define *max-index-1* (-llong *max-index* #l1))
(define *max-index+1* (+llong *max-index* #l1))
(define *max-uint29* (-llong (exptllong #l2 29) #l1))
(define *max-int30* (-llong (exptllong #l2 29) #l1))
(define *min-int30* (negllong (exptllong #l2 29)))
(define *max-int32* (-llong (exptllong #l2 31) #l1))
(define *min-int32* (negllong (exptllong #l2 31)))
(define *max-uint32* (-llong (exptllong #l2 32) #l1))
(define *max-int53* (exptllong #l2 53))
(define *min-int53* (negllong (exptllong #l2 53)))
(define *max-integer* (exptllong #l2 53))
(define *min-integer* (negllong (exptllong #l2 53)))
(define *+inf.0* (exptllong #l2 54))
(define *-inf.0* (negllong (exptllong #l2 54)))

;*---------------------------------------------------------------------*/
;*    j2s-dump-range ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-dump-range rng)

   (define (j2s-dump-range-int int)
      (cond
	 ((=llong int *max-length*) 'length)
	 ((=llong int *max-index*) 'index)
	 ((=llong int *max-index-1*) 'index-1)
	 ((=llong int *max-index+1*) 'index+1)
	 ((=llong int *max-uint29*) 'uint29)
	 ((=llong int *max-int30*) 'max-int30)
	 ((=llong int *min-int30*) 'min-int30)
	 ((=llong int *max-int32*) 'max-int32)
	 ((=llong int *min-int32*) 'min-int32)
	 ((=llong int *max-uint32*) 'max-uint32)
	 ((=llong int *max-int53*) 'max-int53)
	 ((=llong int *min-int53*) 'min-int53)
	 ((=llong int *min-int53*) 'min-int53)
	 ((>=llong int *+inf.0*) '+inf)
	 ((<=llong int *-inf.0*) '-inf)
	 (else int)))

   (if (interval? rng)
       (interval
	  (j2s-dump-range-int (interval-min rng))
	  (j2s-dump-range-int (interval-max rng))
	  (interval-type rng))
       rng))
   
;*---------------------------------------------------------------------*/
;*    dump-range ...                                                   */
;*---------------------------------------------------------------------*/
(define (dump-range this::J2SNode)
   (if (or (>= (bigloo-debug) 2)
	   (let ((env (or (getenv "HOPTRACE") "")))
	      (let ((i (string-contains env "j2s:range")))
		 (when i
		    (or (=fx i (-fx (string-length env) 9))
			(char=? (string-ref env (+fx i 9)) #\space))))))
       (let ((range (cond
		      ((isa? this J2SExpr)
		       (with-access::J2SExpr this (range) range))
		      ((isa? this J2SDecl)
		       (with-access::J2SDecl this (vrange) vrange))
		      (else
		       '()))))
	  (append 
	     (if (interval? range) `(:range ,(j2s-dump-range range)) '())
	     (if (isa? this J2SFun)
		 (with-access::J2SFun this (rrange)
		    (if (interval? rrange)
			`(:rrange ,(j2s-dump-range rrange))
			'()))
		 '())))
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
	      (if cache `(:cache ,cache :cspecs ,cspecs) `(:cspecs ,cspecs))))
	  ((isa? this J2SCall)
	   (with-access::J2SCall this (cache cspecs)
	      (if cache `(:cache ,cache :cspecs ,cspecs) `(:cspecs ,cspecs))))
	  ((isa? this J2SNew)
	   (with-access::J2SNew this (caches)
	      (if (pair? caches) `(:caches ,@caches) '())))
	  ((isa? this J2SAssigOp)
	   (with-access::J2SAssigOp this (cache)
	      (if cache `(:cache ,cache) '())))
	  ((isa? this J2SPrefix)
	   (with-access::J2SPrefix this (cache)
	      (if cache `(:cache ,cache) '())))
	  ((isa? this J2SPostfix)
	   (with-access::J2SPostfix this (cache)
	      (if cache `(:cache ,cache) '())))
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
       (with-access::J2SDecl this (usecnt useinloop escape usage writable scope)
	  `((:writable ,writable)
	    (:usecnt ,usecnt)
	    (:useinloop ,useinloop)
	    (:escape ,escape)
	    (:usage ,(usage->keys usage))
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
   `(alien :typeof ,(string->symbol (typeof this))
       :expr ,(cond
		 ((or (string? this) (symbol? this)
		      (struct? this) (boolean? this)
		      (number? this) (char? this))
		  (format "~a" this))
		 ((list? this)
		  (format "~a" (map j2s->list this)))
		 (else
		  (format "~s" (typeof this))))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::object ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::object)
   (list (typeof this)))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SNode)
   `(,(string->symbol (typeof this))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SMeta ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SMeta)
   (with-access::J2SMeta this (stmt meta optim debug)
      `(,(string->symbol (typeof this)) ,meta :optim ,optim :debug ,debug
	  ,(j2s->list stmt))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SProgram ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SProgram)
   (with-access::J2SProgram this (nodes headers decls mode direct-eval exports)
      `(,(string->symbol (typeof this))
	mode: ,mode
	direct-eval: ,direct-eval
	exports: ,(map j2s->list exports)
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
	,@(dump-size this)
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
	,@(dump-size this)
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
      `(,@(call-next-method) ,@(dump-cache this) ,op)))
  
;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SPrefix ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SPrefix)
   (with-access::J2SPrefix this (lhs rhs loc op)
      `(,@(call-next-method) ,@(dump-cache this) ,@(dump-range this) ,op)))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SPostfix ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SPostfix)
   (with-access::J2SPostfix this (lhs rhs loc op)
      `(,@(call-next-method) ,@(dump-cache this) ,@(dump-range this) ,op)))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SUnresolvedRef ...                                 */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SUnresolvedRef)
   (with-access::J2SUnresolvedRef this (id loc)
      `(,@(call-next-method) ,@(dump-loc loc) ,id
	  ,@(dump-type this))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SGlobalRef ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SGlobalRef)
   (with-access::J2SGlobalRef this (id decl loc)
      (with-access::J2SDecl decl (id usage scope)
	 `(,@(call-next-method)
	     ,@(dump-scope scope)
	     ,@(dump-vtype decl)
	     ,@(dump-access decl)))))
 
;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SCast ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SCast)
   (with-access::J2SCast this (expr type)
      `(,@(call-next-method) :type ,(type->sexp type) ,(j2s->list expr))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SCheck ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SCheck)
   (with-access::J2SCheck this (expr type)
      `(,@(call-next-method) :type ,type ,(j2s->list expr))))

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
	     ,@(dump-hint decl)
	     ,@(dump-range this)))))
 
;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SWithRef ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SWithRef)
   (with-access::J2SWithRef this (id withs expr type)
      `(,@(call-next-method) ,id ,@(dump-type this) ,withs ,(j2s->list expr))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SSuper ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SSuper)
   (with-access::J2SSuper this (context)
      `(,@(call-next-method)
	  :context ,(if (isa? context J2SClass)
			(with-access::J2SClass context (name) name)
			context))))

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
      `(,@(call-next-method) ,@(dump-range this) ,val)))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SLiteralCnst ...                                   */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SLiteralCnst)
   (with-access::J2SLiteralCnst this (index val)
      `(,@(call-next-method)
	  ,@(dump-type this)
	  :index ,index
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
;*    j2s->list ::J2SSpread ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SSpread)
   (with-access::J2SSpread this (expr)
      `(,@(call-next-method) ,(j2s->list expr))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SFun ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SFun)
   (with-access::J2SFun this (name thisp argumentsp params body decl mode
				rutype rtype optimize src 
				need-bind-exit-return new-target idgen
				idthis generator loc vararg constrsize)
      (cond
	 ((isa? decl J2SDeclFun)
	  (with-access::J2SDeclFun decl (key usage scope val key)
	     (unless (eq? val this)
		(error/loc "j2s->list" (format "inconsistent J2SFun (key=~s)" key)
		   name loc))
	     `(,@(call-next-method) ,@(if generator '(*) '())
		 :name ,name :mode ,mode :idgen ,idgen :constrsize ,constrsize
		 ,@(dump-loc loc)
		 ,@(dump-key key)
		 ,@(dump-scope scope)
		 ,@(dump-access decl)
		 ,@(dump-info this)
		 ,@(dump-type this)
		 ,@(dump-rtype this)
		 ,@(dump-rutype this)
		 ,@(dump-need-bind-exit-return need-bind-exit-return)
		 ,@(if optimize '() `(:optimize ,optimize))
		 ,@(dump-new-target new-target)
		 ,@(dump-range this)
		 ,@(if thisp `(:thisp ,(j2s->list thisp)) '())
		 ,@(if idthis `(:idthis ,(j2s->list idthis)) '())
		 ,@(if argumentsp `(:argumentsp ,(j2s->list argumentsp)) '())
		 ,@(if vararg `(:vararg ,vararg) '())
		 ,@(if src '() `(:src #f))
		 ,@(dump-size this)
		 ,(map j2s->list params) ,(j2s->list body))))
	 ((isa? decl J2SDecl)
	  (with-access::J2SDecl decl (key scope)
	     `(,@(call-next-method) ,@(if generator '(*) '())
		 :name ,name :mode ,mode :idgen ,idgen :constrsize ,constrsize
		 ,@(dump-loc loc)
		 ,@(dump-key key)
		 ,@(dump-scope scope)
		 ,@(dump-access decl)
		 ,@(dump-info this)
		 ,@(dump-type this)
		 ,@(dump-rtype this)
		 ,@(dump-rutype this)
		 ,@(dump-need-bind-exit-return need-bind-exit-return)
		 ,@(if optimize '() `(:optimize ,optimize))
		 ,@(if src '() `(:src #f))
		 ,@(dump-new-target new-target)
		 ,@(dump-range this)
		 ,@(if thisp `(:thisp ,(j2s->list thisp)) '())
		 ,@(if idthis `(:idthis ,(j2s->list idthis)) '())
		 ,@(if argumentsp `(:argumentsp ,(j2s->list argumentsp)) '())
		 ,@(if vararg `(:vararg ,vararg) '())
		 ,(map j2s->list params) ,(j2s->list body))))
	 (else
	  `(,@(call-next-method) ,@(if generator '(*) '())
	      :name ,name :mode ,mode :idgen ,idgen :constrsize ,constrsize
	      ,@(dump-loc loc)
	      ,@(dump-info this)
	      ,@(dump-type this)
	      ,@(dump-rtype this)
	      ,@(dump-rutype this)
	      ,@(dump-need-bind-exit-return need-bind-exit-return)
	      ,@(if optimize '() `(:optimize ,optimize))
	      ,@(if src '() `(:src #f))
	      ,@(dump-new-target new-target)
	      ,@(dump-range this)
	      ,@(if thisp `(:thisp ,(j2s->list thisp)) '())
	      ,@(if idthis `(:idthis ,(j2s->list idthis)) '())
	      ,@(if argumentsp `(:argumentsp ,(j2s->list argumentsp)) '())
	      ,@(if vararg `(:vararg ,vararg) '())
	      ,(map j2s->list params) ,(j2s->list body))))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SMethod ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SMethod)
   (with-access::J2SMethod this (function method loc)
      `(,@(call-next-method)
	  ,@(dump-loc loc)
	  :function ,(j2s->list function)
	  :method ,(j2s->list method))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SBindExit ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SBindExit)
   (with-access::J2SBindExit this (lbl stmt loc)
      `(,@(call-next-method)
	  ,@(dump-type this)
	  ,@(dump-hint this)
	  ,@(dump-loc loc)
	  ,@(dump-range this)
	  (,lbl) ,(j2s->list stmt))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SReturn ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SReturn)
   (with-access::J2SReturn this (expr tail from loc)
      `(,@(call-next-method)
	  ,@(dump-loc loc)
	  ,@(dump-size this)
	  ,@(dump-from from)
	  ,@(if (isa? from J2SFun)
		(dump-rtype from)
		'())
	  ,@(if (isa? from J2SBindExit)
		(with-access::J2SBindExit from (lbl type)
		   (list :from lbl :type type))
		'())
	  ,@(dump-tail tail)
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
	  ,@(dump-size this)
	  ,@(dump-type this)
	  ,@(dump-info this)
	  ,@(dump-hint this)
	  ,@(dump-range this)
	  ,(j2s->list lhs) ,(j2s->list rhs))))
      
;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SParen ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SParen)
   (with-access::J2SParen this (expr loc)
      `(,@(call-next-method)
	  ,@(dump-loc loc)
	  ,@(dump-size this)
	  ,@(dump-type this)
	  ,@(dump-range this)
	  ,@(dump-hint this)
	  ,(j2s->list expr))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SUnary ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SUnary)
   (with-access::J2SUnary this (op expr)
      `(,@(call-next-method) ,op
	  ,@(dump-type this)
	  ,@(dump-size this)
	  ,@(dump-info this)
	  ,@(dump-hint this)
	  ,(j2s->list expr))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SIf ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SIf)
   (with-access::J2SIf this (test then else loc)
      `(,@(call-next-method)
	  ,@(dump-loc loc)
	  ,@(dump-size this)
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
   (with-access::J2SCond this (test then else loc)
      `(,@(call-next-method)
	  ,@(dump-loc loc)
	  ,@(dump-info this)
	  ,@(dump-size this)
	  ,@(dump-type this)
	  ,@(dump-range this)
	  ,(j2s->list test)
	  ,(j2s->list then)
	  ,(j2s->list else) )))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SWhile ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SWhile)
   (with-access::J2SWhile this (op test body loc)
      `(,@(call-next-method) ,@(dump-loc loc) ,@(dump-info this)
	  ,@(dump-size this)
	  ,(j2s->list test) ,(j2s->list body))))
   
;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SFor ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SFor)
   (with-access::J2SFor this (init test incr body loc)
      `(,@(call-next-method) ,@(dump-loc loc) ,@(dump-info this)
	  ,@(dump-size this)
	  ,(j2s->list init)
	  ,(j2s->list test)
	  ,(j2s->list incr)
	  ,(j2s->list body))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SForIn ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SForIn)
   (with-access::J2SForIn this (lhs obj body loc)
      `(,@(call-next-method) ,@(dump-loc loc) ,@(dump-info this)
	  ,@(dump-size this)
	  ,(j2s->list lhs)
	  ,(j2s->list obj)
	  ,(j2s->list body))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SLabel ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SLabel)
   (with-access::J2SLabel this (lhs id body)
      `(,@(call-next-method) ,id ,(j2s->list body))))
   
;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SBreak ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SBreak)
   (with-access::J2SBreak this (loc id target)
      `(,@(call-next-method) ,id :target ,(typeof target))))

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
   (with-access::J2SCall this (fun thisarg args loc protocol profid)
      `(,@(call-next-method)
	  ,@(dump-loc loc)
	  ,@(dump-size this)
	  ,@(dump-type this)
	  ,@(dump-rtype fun)
	  ,@(dump-info this)
	  ,@(dump-hint this)
	  ,@(dump-range this)
	  ,@(dump-protocol protocol)
	  ,@(if (or (>= (bigloo-debug) 2)
		  (string-contains (or (getenv "HOPTRACE") "") "j2s:profid"))
		`(:profid ,profid)
		'())
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
	  ,@(dump-size this)
	  ,@(dump-type this)
	  ,@(dump-info this)
	  ,@(dump-hint this)
	  ,@(dump-range this)
	  ,@(dump-cache this)
	  ,(j2s->list obj) ,(j2s->list field))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SStmtExpr ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SStmtExpr)
   (with-access::J2SStmtExpr this (expr loc)
      `(,@(call-next-method) ,@(dump-loc loc)
	  ,@(dump-info this) ,(j2s->list expr))))
   
;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SObjectInit ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SObjInit)
   (with-access::J2SObjInit this (inits ronly)
      `(,@(call-next-method)
	  ,@(if (or (string-contains (or (getenv "HOPTRACE") "") "j2s:usage")
		    (string-contains (or (getenv "HOPTRACE") "") "j2s:access"))
		`(:ronly ,ronly)
		'())
	  ,@(map j2s->list inits))))

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
   (with-access::J2SDecl this (id key binder _scmid usage scope loc)
      `(,(string->symbol (format "~a/~a" (typeof this) binder))
	,id
	,@(dump-loc loc)
	,@(dump-dump this)
	,@(dump-key key)
	,@(dump-access this)
	,@(dump-vtype this)
	,@(dump-hint this)
	,@(dump-range this)
	,@(if _scmid `(:_scmid ,_scmid) '())
	,@(dump-info this)
	,@(dump-scope scope))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SDeclRest ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SDeclRest)
   (with-access::J2SDeclRest this (id key _scmid usage alloc-policy)
      `(,(string->symbol (typeof this))
	,id
	,@(dump-dump this)
	,@(dump-key key)
	,@(dump-access this)
	,@(dump-vtype this)
	,@(if _scmid `(:_scmid ,_scmid) '())
	:alloc-policy ,alloc-policy
	,@(dump-info this))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SDeclArguments ...                                 */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SDeclArguments)
   (with-access::J2SDeclArguments this (id key _scmid usage alloc-policy argid)
      `(,(string->symbol (typeof this))
	,id
	,@(dump-dump this)
	,@(dump-key key)
	,@(dump-access this)
	,@(dump-vtype this)
	,@(if _scmid `(:_scmid ,_scmid) '())
	:argid ,argid :alloc-policy ,alloc-policy
	,@(dump-info this))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SDeclInit ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SDeclInit)
   (with-access::J2SDeclInit this (val loc)
      `(,@(call-next-method)
	  ,@(dump-loc loc)
	  ,@(if (nodefval? val) '() (list (j2s->list val))))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SPragma ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SPragma)
   (with-access::J2SPragma this (expr vars vals type)
      `(,@(call-next-method) ,@(dump-type this)
	  ,@(if (pair? vars) `(:vars ,vars) '())
	  ,@(if (pair? vals) `(:vals ,(map j2s->list vals)) '())
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
   (with-access::J2SNew this (clazz args cache protocol)
      `(,@(call-next-method) ,@(dump-type this)
	  ,@(dump-protocol protocol)
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
	  ,@(dump-size this)
	  ,@(map j2s->list cases))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SCase ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SCase)
   (with-access::J2SCase this (expr body cascade)
      (append (call-next-method)
	 (if (>= (bigloo-debug) 3) `(:cascade ,cascade) '())
	 (dump-size this)
	 (list (j2s->list expr) (j2s->list body)))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SDefault ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SDefault)
   (with-access::J2SDefault this (body)
      (list 'J2SDefault (j2s->list body))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SClass ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SClass)
   (with-access::J2SClass this (name super elements decl loc need-dead-zone-check)
      `(,(string->symbol (typeof this))
	,@(if name (list :name name) '())
	:super ,(j2s->list super)
	:need-dead-zone-check ,need-dead-zone-check
	,@(dump-loc loc)
	,@(dump-type this)
	,@(if (isa? decl J2SDecl)
	      (with-access::J2SDecl decl (key)
		 (append (dump-key key) (dump-vtype decl)))
	      '())
	,@(map j2s->list elements))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SClassElement ...                                  */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SClassElement)
   (with-access::J2SClassElement this (prop static type)
      `(J2SClassElement :static ,static
	  ,@(if (or (>= (bigloo-debug) 2)
		    (string-contains (or (getenv "HOPTRACE") "") "j2s:type"))
		`(:type ,(type->sexp type))
		'())
	  ,(j2s->list prop))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SDProducer ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SDProducer)
   (with-access::J2SDProducer this (decl expr size)
      (with-access::J2SDecl decl (id)
	 `(,@(call-next-method) ,id ,@(dump-type this) ,size
	     ,(j2s->list expr)))))
		  
;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SDConsumer ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SDConsumer)
   (with-access::J2SDConsumer this (expr path)
      `(,@(call-next-method) ,@(dump-type this) ,path ,(j2s->list expr))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SCacheUpdate ...                                   */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SCacheUpdate)
   (with-access::J2SCacheUpdate this (prop cache obj)
      `(,@(call-next-method) :cache ,cache ,prop ,(j2s->list obj))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SCacheCheck ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SCacheCheck)
   (with-access::J2SCacheCheck this (prop owner cache obj fields)
      `(,@(call-next-method) ,prop :cache ,cache
	  :owner ,(cond
		     ((isa? owner J2SRecord) (type->sexp owner))
		     ((isa? owner J2SNode) (j2s->list owner))
		     (else #f))
	  ,(j2s->list obj)
	  ,@(map j2s->list fields))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SOptInitSeq ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SOPTInitSeq)
   (with-access::J2SOPTInitSeq this (ref offset)
      `(,@(call-next-method) :ref ,(j2s->list ref) :offset ,offset)))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2STemplate ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2STemplate)
   (with-access::J2STemplate this (exprs)
      `(,@(call-next-method) ,@(map j2s->list exprs))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SImport ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SImport)
   (with-access::J2SImport this (path names)
      `(,@(call-next-method) :path ,path
	  :names ,@(map j2s->list names))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SImportExports ...                                 */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SImportExports)
   (with-access::J2SImportExports this (import)
      `(,@(call-next-method)
	  ,(with-access::J2SImport import (path)
	      `(J2SImport :path ,path |...|)))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SImportName ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SImportName)
   (with-access::J2SImportName this (loc id alias)
      `(J2SImportName ,id ,alias)))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SImportRedirect ...                                */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SImportRedirect)
   (with-access::J2SImportRedirect this (loc id alias)
      `(J2SImportRedirect ,id ,alias)))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SImportNamespace ...                               */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SImportNamespace)
   (with-access::J2SImportNamespace this (loc id)
      `(J2SImportNamespace ,id)))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SImportExport ...                                  */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SImportExport)
   (with-access::J2SImportExport this (loc)
      '(J2SImportExport)))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SExportVars ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SExportVars)
   (with-access::J2SExportVars this (refs)
      `(,@(call-next-method) ,@(map j2s->list refs))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SExport ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SExport)
   (with-access::J2SExport this (id alias index from writable)
      `(,@(call-next-method) ,id ,@(if (eq? alias id) '() `(:alias ,alias))
	  index: ,index
	  writable: writable
	  from: ,(typeof from))))

;*---------------------------------------------------------------------*/
;*    j2ssum ...                                                       */
;*---------------------------------------------------------------------*/
(define-generic (j2ssum this::obj)
   (if (pair? this)
       (apply + (map j2ssum this))
       0))

;*---------------------------------------------------------------------*/
;*    j2ssum ::J2SNode ...                                             */
;*---------------------------------------------------------------------*/
(define-method (j2ssum this::J2SNode)
   (let* ((clazz (object-class this))
	  (fields (class-all-fields clazz)))
      (let loop ((i (-fx (vector-length fields) 1))
		 (sum (get-hashnumber (class-name clazz))))
	 (if (=fx i -1)
	     sum
	     (let* ((f (vector-ref-ur fields i))
		    (fi (class-field-info f)))
		(if (and (pair? fi) (member "notraverse" fi))
		    (loop (-fx i 1) sum)
		    (let ((v ((class-field-accessor f) this)))
		       (loop (-fx i 1) (+fx 1 (+fx (j2ssum v) sum))))))))))

;*---------------------------------------------------------------------*/
;*    j2ssum ::J2SLiteralValue ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2ssum this::J2SLiteralValue)
   (with-access::J2SLiteralValue this (val)
      (get-hashnumber val)))

;*---------------------------------------------------------------------*/
;*    j2ssum ::J2SDProducer ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2ssum this::J2SDProducer)
   (with-access::J2SDProducer this (expr size)
      (+fx 3 (+fx size (j2ssum expr)))))
