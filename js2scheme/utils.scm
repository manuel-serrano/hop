;*=====================================================================*/
;*    serrano/prgm/project/hop/3.5.x/js2scheme/utils.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 13 16:59:06 2013                          */
;*    Last change :  Sat Jan 15 06:41:32 2022 (serrano)                */
;*    Copyright   :  2013-22 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Utility functions                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_utils
   
   (include "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump)
   
   (export (pass ::bstring)
	   (pp/width ::obj ::output-port #!optional (width 256))
	   (error/loc proc obj msg loc)
	   (illegal-node ::bstring ::J2SNode)
	   (config-get ::pair-nil ::keyword #!optional def)
	   (config-get-mmap ::pair-nil path)
	   (config-put! ::pair-nil ::keyword ::obj)
	   (this?::bool ::J2SNode)
	   
	   (j2s-expression-src loc ::pair-nil ::bstring)
	   
	   (m64? ::pair-nil)
	   (u32? ::pair-nil)
	   (conf-max-int::llong ::pair-nil)
	   (conf-min-int::llong ::pair-nil)
	   
	   (type-int32?::bool ::obj)
	   (type-uint32?::bool ::obj)
	   (type-int30?::bool ::obj)
	   (type-int53?::bool ::obj)
	   (type-fixnum?::bool ::obj)
	   (type-integer?::bool ::obj)
	   (type-number?::bool ::obj)
	   (type-object?::bool ::obj)
	   (type-maybe?::bool ::obj ::pair-nil)
	   (type-cannot?::bool ::obj ::pair-nil)
	   (type-subtype?::bool ::obj ::obj)
	   (type-maybe-subtype?::bool ::obj ::obj)
	   (type-name type #!optional (conf '()))
	   
	   (min-type::obj ::obj ::obj)
	   (max-type::obj ::obj ::obj)
	   (js-uint32-tointeger expr conf)
	   
	   (j2s-expr-type-test ::J2SExpr)
	   
	   (j2s-type ::obj)
	   (j2s-vtype ::obj)
	   (j2s-mtype ::obj)
	   (j2s-etype ::obj ::pair-nil)
	   
	   (class-of ::J2SExpr)
	   
	   (best-hint::pair ::J2SExpr)
	   (is-hint?::bool ::J2SExpr ::symbol)
	   (is-not-hint?::bool ::J2SExpr ::symbol)
	   
	   (string-method-type name #!optional (default '(any any)))
	   (string-static-method-type name #!optional (default '(any any)))
	   (math-static-method-type name #!optional (default '(any any)))
	   (object-static-method-type name #!optional (default '(any any)))
	   (regexp-method-type name #!optional (default '(any any)))
	   (map-method-type name #!optional (default '(any any)))
	   (weakmap-method-type name #!optional (default '(any any)))
	   (number-method-type name #!optional (default '(any any)))
	   (array-method-type name #!optional (default '(any any)))
	   (jsvector-method-type name #!optional (default '(any any)))
	   
	   (find-builtin-method-type ::J2SExpr ::bstring)
	   (guess-builtin-method-type ::J2SExpr ::bstring)
	   
	   (is-builtin-ref?::bool ::J2SExpr ::symbol)
	   (constructor-only?::bool ::J2SDeclFun)
	   (constructor-no-return?::bool ::J2SDeclFun)))

;*---------------------------------------------------------------------*/
;*    pass ...                                                         */
;*---------------------------------------------------------------------*/
(define (pass name)
   (print name))

;*---------------------------------------------------------------------*/
;*    pp/width ...                                                     */
;*---------------------------------------------------------------------*/
(define (pp/width obj out #!optional (width 256))
   (let ((w *pp-width*))
      (set! *pp-width* width)
      (pp obj out)
      (set! *pp-width* w)))

;*---------------------------------------------------------------------*/
;*    error/loc ...                                                    */
;*---------------------------------------------------------------------*/
(define (error/loc proc obj msg loc)
   (match-case loc
      ((at ?fname ?point)
       (error/location proc obj msg fname point))
      (else
       (error proc obj msg))))

;*---------------------------------------------------------------------*/
;*    illegal-node ...                                                 */
;*---------------------------------------------------------------------*/
(define (illegal-node pass this::J2SNode)
   (with-access::J2SNode this (loc)
      (error/loc pass
	 (format "~a should have been eliminated" (typeof this))
	 (j2s->sexp this)
	 loc)))

;*---------------------------------------------------------------------*/
;*    config-get ...                                                   */
;*---------------------------------------------------------------------*/
(define (config-get conf k #!optional def)
   (let ((l (memq k conf)))
      (if (pair? l)
	  (cadr l)
	  def)))

;*---------------------------------------------------------------------*/
;*    config-get-mmap ...                                              */
;*---------------------------------------------------------------------*/
(define (config-get-mmap conf path)
   (if (string=? path (config-get conf :filename))
       (config-get conf :mmap-src)
       (let ((mmaps (config-get conf :mmaps)))
	  (let ((m (assoc path mmaps)))
	     (cond
		((pair? m)
		 (cdr m))
		((file-exists? path)
		 (let ((mmap (open-mmap path :write #f)))
		    (config-put! conf :mmaps (cons (cons path mmap) mmaps))
		    mmap))
		(else
		 #f))))))
	      
;*---------------------------------------------------------------------*/
;*    config-put! ...                                                  */
;*---------------------------------------------------------------------*/
(define (config-put! conf k val)
   (let ((l (memq k conf)))
      (if (pair? l)
	  (set-car! (cdr l) val)
	  (error "config-put!" (format "entry `~a' not in conf" k) conf))))

;*---------------------------------------------------------------------*/
;*    this? ...                                                        */
;*    -------------------------------------------------------------    */
;*    true iff the body uses the "this" pseudo variable                */
;*---------------------------------------------------------------------*/
(define (this? body)
   (let ((res (make-cell #f)))
      (use-this? body res)
      (cell-ref res)))

;*---------------------------------------------------------------------*/
;*    use-this? ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (use-this? this::J2SNode res)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    this? ::J2SThis ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (use-this? this::J2SThis res)
   (cell-set! res #t))
   
;*---------------------------------------------------------------------*/
;*    this? ::J2SThis ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (use-this? this::J2SFun res)
   #f)

;*---------------------------------------------------------------------*/
;*    j2s-expression-src ...                                           */
;*---------------------------------------------------------------------*/
(define (j2s-expression-src loc conf default::bstring)
   
   (define delims
      '(#\space #\newline #\tab #\; #\{ #\} #\( #\) #\* #\+ #\- #\/))
   
   (define (find-delim mmap start #!optional (max #e20))
      (let ((end (minelong (mmap-length mmap) (+elong start max))))
	 (let loop ((i (+elong start 1)))
	    (cond
	       ((>=elong i end) i)
	       ((memq (mmap-ref mmap i) delims) i)
	       (else (loop (+elong i #e1)))))))
   
   (match-case loc
      ((at ?path ?start)
       (let ((m (config-get-mmap conf path)))
	  (if (mmap? m)
	      (let ((end (find-delim m (fixnum->elong start))))
		 (if (>elong end (fixnum->elong start))
		     (mmap-substring m (fixnum->elong start) end)
		     default))
	      default)))
      (else
       default)))

;*---------------------------------------------------------------------*/
;*    m64? ...                                                         */
;*---------------------------------------------------------------------*/
(define (m64? conf)
   (>=fx (config-get conf :int-size 0) 53))

;*---------------------------------------------------------------------*/
;*    u32? ...                                                         */
;*---------------------------------------------------------------------*/
(define (u32? conf)
   (>=fx (config-get conf :optim 0) 4))

;*---------------------------------------------------------------------*/
;*    conf-max-int ...                                                 */
;*---------------------------------------------------------------------*/
(define (conf-max-int::llong conf)
   (let ((shift (config-get conf :int-size 30)))
      (-llong (bit-lshllong #l1 (fixnum->llong (-fx shift 1))) #l1)))
   
;*---------------------------------------------------------------------*/
;*    conf-min-int ...                                                 */
;*---------------------------------------------------------------------*/
(define (conf-min-int::llong conf)
   (let ((shift (config-get conf :int-size 30)))
      (negllong (bit-lshllong #l1 (fixnum->llong (-fx shift 1))))))

;*---------------------------------------------------------------------*/
;*    type-uint32? ...                                                 */
;*---------------------------------------------------------------------*/
(define (type-uint32? type)
   (memq type '(index uint32 length)))

;*---------------------------------------------------------------------*/
;*    type-int32? ...                                                  */
;*---------------------------------------------------------------------*/
(define (type-int32? type)
   (memq type '(uint29 int30 int32)))

;*---------------------------------------------------------------------*/
;*    type-int30? ...                                                  */
;*---------------------------------------------------------------------*/
(define (type-int30? type)
   (memq type '(uint29 int30)))

;*---------------------------------------------------------------------*/
;*    type-int53? ...                                                  */
;*---------------------------------------------------------------------*/
(define (type-int53? type)
   (memq type '(uint29 int30 int32 int53 ufixnum index)))

;*---------------------------------------------------------------------*/
;*    type-fixnum? ...                                                 */
;*---------------------------------------------------------------------*/
(define (type-fixnum? type)
   (memq type '(int32 uint32 integer bint)))

;*---------------------------------------------------------------------*/
;*    type-integer? ...                                                */
;*---------------------------------------------------------------------*/
(define (type-integer? type)
   (or (type-int53? type)
       (type-uint32? type)
       (memq type '(integer fixnum ufixnum))))
   
;*---------------------------------------------------------------------*/
;*    type-number? ...                                                 */
;*---------------------------------------------------------------------*/
(define (type-number? type)
   (or (type-integer? type) (memq type '(real number))))

;*---------------------------------------------------------------------*/
;*    type-object? ...                                                 */
;*---------------------------------------------------------------------*/
(define (type-object? type)
   (or (memq type '(object regexp date Promise array jsvector function arguments global this map weakmap set weakset))
       (isa? type J2SClass)))

;*---------------------------------------------------------------------*/
;*    type-maybe? ...                                                  */
;*---------------------------------------------------------------------*/
(define (type-maybe? type types::pair-nil)
   (cond
      ((memq type '(any unknown)) #t)
      ((memq type types) #t)
      ((eq? type 'number)
       (any (lambda (t) (type-maybe? t types))
	  '(real int30 int32 uint32 int53 bint fixnum ufixnum integer)))
      (else #f)))

;*---------------------------------------------------------------------*/
;*    type-cannot? ...                                                 */
;*---------------------------------------------------------------------*/
(define (type-cannot? type types::pair-nil)
   (not (type-maybe? type types)))

;*---------------------------------------------------------------------*/
;*    noclass-subtype? ...                                             */
;*---------------------------------------------------------------------*/
(define (noclass-subtype? type supertype)
   (or (eq? supertype 'any)
       (eq? type supertype)
       (and (eq? supertype 'number)
	    (memq type '(integer real)))
       (and (eq? supertype 'object)
	    (memq type '(array jsvector date regexp function argument promise)))
       (and (eq? supertype 'object)
	    (type-object? type))
       (and (eq? type 'integer) (eq? supertype 'number))
       (and (eq? type 'function) (eq? supertype 'arrow))
       (and (memq type '(record class)) (eq? supertype 'function))
       (and (memq type '(index length indexof)) (memq supertype '(integer number)))))

;*---------------------------------------------------------------------*/
;*    type-subtype? ...                                                */
;*---------------------------------------------------------------------*/
(define (type-subtype? type supertype)
   
   (define (class-subtype? type supertype)
      (or (eq? type supertype)
	  (with-access::J2SClass type (super)
	     (when (isa? super J2SRef)
		(with-access::J2SRef super (decl)
		   (when (isa? decl J2SDeclClass)
		      (with-access::J2SDeclClass decl (val)
			 (when (isa? val J2SClass)
			    (class-subtype? val supertype)))))))))

   (or (noclass-subtype? type supertype)
       (and (isa? type J2SClass)
	    (or (eq? supertype 'object)
		(and (isa? supertype J2SClass)
		     (class-subtype? type supertype))))))

;*---------------------------------------------------------------------*/
;*    type-maybe-subtype? ...                                          */
;*    -------------------------------------------------------------    */
;*    As type-subtype? but returns #t if the max-super class is        */
;*    unknown or a function.                                           */
;*---------------------------------------------------------------------*/
(define (type-maybe-subtype? type supertype)
   
   (define (class-subtype? type supertype)
      (or (eq? type supertype)
	  (with-access::J2SClass type (super)
	     (cond
		((isa? super J2SRef)
		 (with-access::J2SRef super (decl)
		    (if (isa? decl J2SDeclClass)
			(with-access::J2SDeclClass decl (val)
			   (when (isa? val J2SClass)
			      (class-subtype? val supertype)))
			#t)))
		((or (isa? super J2SUndefined) (isa? super J2SNull))
		 #f)
		(else
		 #t)))))
   
   (or (eq? supertype 'unknown)
       (noclass-subtype? type supertype)
       (and (isa? supertype J2SClass)
	    (or (memq type '(any unknown obj object))
		(and (isa? type J2SClass) (class-subtype? type supertype))))
       (and (isa? type J2SClass)
	    (class-subtype? type supertype))))

;*---------------------------------------------------------------------*/
;*    type-name ...                                                    */
;*---------------------------------------------------------------------*/
(define (type-name type #!optional (conf '()))
   (cond
      ((symbol? type)
       (case type
	  ((int30 int32) 'int32)
	  ((uint32) 'uint32)
	  ((int53) (if (m64? conf) 'long 'obj))
	  ((bint) 'bint)
	  ((unknown any number) 'obj)
	  ((int30 fixnum ufixnum) 'long)
	  ((boolean) 'bool)
	  ((integer) 'obj)
	  ((object this) 'JsObject)
	  ((undefined) 'unspecified)
	  ((regexp) 'JsRegExp)
	  ((array) 'JsArray)
	  ((jsvector) 'JsArray)
	  ((int8array) 'JsInt8Array)
	  ((uint8array) 'JsUint8Array)
	  ((int16array) 'JsInt16Array)
	  ((uint16array) 'JsUint16Array)
	  ((int32array) 'JsInt32Array)
	  ((uint32array) 'JsUint32Array)
	  ((float32array) 'JsFloat32Array)
	  ((float64array) 'JsFloat64Array)
	  ((function) 'JsFunction)
	  ((arrow) 'JsProcedure)
	  ((service) 'JsService)
	  ((date) 'JsDate)
	  ((string buffer) 'JsStringLiteral)
	  ((null) 'nil)
	  ((String) 'JsString)
	  ((Promise) 'JsPromise)
	  ((class) 'JsFunction)
	  ((arguments) 'JsArguments)
	  ((real) 'double)
	  ((bigint) 'bignum)
	  ((map weakmap) 'JsMap)
	  ((set weakset) 'JsMap)
	  (else type)))
      ((isa? type J2SRecord)
       (with-access::J2SRecord type (name)
	  (string->symbol (string-append "&JsRec" (symbol->string! name)))))
      ((isa? type J2SClass)
       'JsObject)
      (else
       (error "type-name" "Illegal type" type))))
   
;*---------------------------------------------------------------------*/
;*    min-type ...                                                     */
;*    -------------------------------------------------------------    */
;*    Return the smallest type that can represent both types.          */
;*---------------------------------------------------------------------*/
(define (min-type t1 t2)
   (cond
      ((eq? t1 t2) t1)
      ((eq? t1 'unknown) t2)
      ((eq? t2 'unknown) t2)
      ((type-subtype? t1 t2) t1)
      ((type-subtype? t2 t1) t2)
      (else
       (case t1
	  ((index) t1)
	  ((length) (if (eq? t2 'index) 'index t1))
	  ((uint32) (if (memq t2 '(index length)) t2 t1))
	  ((int32) t1)
	  ((int53) (if (eq? t2 'int32) t2 t2))
	  ((integer) (if (memq t2 '(int32 uint32)) t2 t1))
	  ((number integer) t1)
	  (else 'any)))))

;*---------------------------------------------------------------------*/
;*    max-type ...                                                     */
;*    -------------------------------------------------------------    */
;*    Return the biggest type that can represent both types.           */
;*---------------------------------------------------------------------*/
(define (max-type t1 t2)
   (if (eq? t1 t2)
       t1
       (case t1
	  ((index)
	   t2)
	  ((length)
	   (if (eq? t2 'index) t1 t2))
	  ((int32)
	   (if (memq t2 '(index length)) 'integer t2))
	  ((uint32)
	   (cond
	      ((memq t2 '(index length)) t1)
	      ((memq t2 '(integer number)) 'number)
	      (else 'any)))
	  ((int53)
	   (cond
	      ((memq t2 '(index length int32 uint32)) t1)
	      ((memq t2 '(integer number)) 'number)
	      (else 'any)))
	  ((integer)
	   (cond
	      ((memq t2 '(index length int32 uint32)) t1)
	      ((eq? t2 'number) 'number)
	      (else 'any)))
	  (else
	   'any))))

;*---------------------------------------------------------------------*/
;*    js-uint32-tointeger ...                                          */
;*---------------------------------------------------------------------*/
(define (js-uint32-tointeger expr conf)
   (let ((lgsz (config-get conf :int-size 30)))
      (cond
	 ((and (uint32? expr) (<u32 expr (bit-lshu32 #u32:1 (-fx lgsz 1))))
	  (uint32->fixnum expr))
	 ((>fx lgsz 32)
	  `(uint32->fixnum ,expr))
	 (else
	  `(js-uint32-tointeger ,expr)))))

;*---------------------------------------------------------------------*/
;*    j2s-expr-type-test ...                                           */
;*    -------------------------------------------------------------    */
;*    Is an expression a type test. If it is returns                   */
;*       <op, decl, type, ref>                                         */
;*    Otherwise, returns #f                                            */
;*    Tested patterns are:                                             */
;*       pat ::= (typeof X == STRING)                                  */
;*           | !pat                                                    */
;*           | (pat)                                                   */
;*---------------------------------------------------------------------*/
(define (j2s-expr-type-test expr::J2SExpr)
   
   (define (not-op op)
      (case op
	 ((==) '!=)
	 ((===) '!==)
	 ((!=) '=)
	 ((!==) '==)
	 ((instanceof) '!instanceof)
	 (else (error "j2s-expr-type-test" "Unknown op" op))))
   
   (define (string->typename val)
      (let ((s (string->symbol val)))
	 (if (eq? s 'boolean)
	     'bool
	     s)))
   
   (define (typeof op expr str)
      (when (isa? expr J2SUnary)
	 (with-access::J2SUnary expr ((bop op) expr)
	    (let loop ((expr expr))
	       (cond
		  ((isa? expr J2SParen)
		   (with-access::J2SParen expr (expr)
		      (loop expr)))
		  ((and (eq? bop 'typeof) (isa? expr J2SRef))
		   (with-access::J2SRef expr (decl)
		      (with-access::J2SString str (val)
			 (values op decl (string->typename val) expr)))))))))
   
   (define (binary-type-test expr)
      (with-access::J2SBinary expr (op lhs rhs)
	 (case op
	    ((== === != !==)
	     (cond
		((isa? lhs J2SString)
		 (typeof op rhs lhs))
		((isa? rhs J2SString)
		 (typeof op lhs rhs))
		((isa? rhs J2SNull)
		 (when (and (isa? lhs J2SRef) (memq op '(=== !==)))
		    (with-access::J2SRef lhs (decl)
		       (values op decl 'null lhs))))
		((isa? rhs J2SUndefined)
		 (when (and (isa? lhs J2SRef) (memq op '(=== !==)))
		    (with-access::J2SRef lhs (decl)
		       (values op decl 'undefined lhs))))
		((isa? lhs J2SNull)
		 (when (and (isa? rhs J2SRef) (memq op '(=== !==)))
		    (with-access::J2SRef rhs (decl)
		       (values op decl 'null rhs))))
		((isa? lhs J2SUndefined)
		 (when (and (isa? rhs J2SRef) (memq op '(=== !==)))
		    (with-access::J2SRef rhs (decl)
		       (values op decl 'undefined rhs))))
		(else
		 #f)))
	    ((instanceof)
	     (when (isa? lhs J2SRef)
		(let ((typ (class-of rhs)))
		   (when typ
		      (with-access::J2SRef lhs (decl)
			 (values 'instanceof decl typ lhs))))))
	    (else
	     #f))))
   
   (define (unary-type-test expr)
      (with-access::J2SUnary expr (op expr)
	 (when (eq? op '!)
	    (multiple-value-bind (op decl type expr)
	       (j2s-expr-type-test expr)
	       (when op
		  (values (not-op op) decl type expr))))))
   
   (define (paren-type-test expr)
      (with-access::J2SParen expr (expr)
	 (j2s-expr-type-test expr)))
   
   (define (is-native-test test)
      ;; if test === (js-index? (ref decl) ) return decl
      ;; see __js2scheme_range
      (when (isa? test J2SCall)
         (with-access::J2SCall test (fun args)
            (when (isa? fun J2SHopRef)
	       (when (and (pair? args) (null? (cdr args)))
		  (when (isa? (car args) J2SRef)
		     (car args)))))))
   
   (define (native-type-test test)
      (with-access::J2SCall test (fun)
	 (with-access::J2SHopRef fun (id)
	    id)))
   
   (cond
      ((isa? expr J2SBinary)
       (binary-type-test expr))
      ((isa? expr J2SUnary)
       (unary-type-test expr))
      ((isa? expr J2SParen)
       (paren-type-test expr))
      ((is-native-test expr)
       =>
       (lambda (ref)
	  (with-access::J2SRef ref (decl)
	     (case (native-type-test expr)
		((js-index?) (values '== decl 'index ref))
		((fixnum?) (values '== decl 'integer ref))
		((number?) (values '== decl 'number ref))
		((js-jsstring?) (values '== decl 'string ref))
		((js-array?) (values '== decl 'array ref))
		((js-function?) (values '== decl 'function ref))
		((js-procedure?) (values '== decl 'arrow ref))
		((boolean?) (values '== decl 'bool ref))
		((js-undefined?) (values '== decl 'undefined ref))
		((js-null?) (values '== decl 'null ref))
		((js-object?) (values '<= decl 'object ref))
		(else #f)))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    j2s-type ...                                                     */
;*---------------------------------------------------------------------*/
(define (j2s-type node)
   (cond
      ((isa? node J2SExpr)
       (with-access::J2SExpr node (type)
	  type))
      (else
       'void)))

;*---------------------------------------------------------------------*/
;*    j2s-vtype ...                                                    */
;*---------------------------------------------------------------------*/
(define (j2s-vtype node)
   (cond
      ((isa? node J2SRef)
       (with-access::J2SRef node (decl)
	  (with-access::J2SDecl decl (vtype)
	     vtype)))
      ((isa? node J2SGlobalRef)
       (with-access::J2SGlobalRef node (decl)
	  (with-access::J2SDecl decl (vtype)
	     vtype)))
      ((isa? node J2SHopRef)
       (with-access::J2SHopRef node (type)
	  type))
      ((isa? node J2SParen)
       (with-access::J2SParen node (expr)
	  (j2s-vtype expr)))
      ((isa? node J2SAssig)
       (with-access::J2SAssig node (lhs)
	  (j2s-vtype lhs)))
      ((isa? node J2SExpr)
       (with-access::J2SExpr node (type)
	  type))
      (else
       'void)))

;*---------------------------------------------------------------------*/
;*    j2s-mtype ...                                                    */
;*---------------------------------------------------------------------*/
(define (j2s-mtype node)
   (cond
      ((isa? node J2SDecl)
       (with-access::J2SDecl node (mtype)
	  mtype))
      ((isa? node J2SRef)
       (with-access::J2SRef node (decl)
	  (j2s-mtype decl)))
      (else
       'unknown)))

;*---------------------------------------------------------------------*/
;*    j2s-etype ...                                                    */
;*    -------------------------------------------------------------    */
;*    The type of an expression.                                       */
;*---------------------------------------------------------------------*/
(define (j2s-etype node conf)
   (let ((vtype (j2s-vtype node)))
      (if (memq vtype '(int32 uint32))
	  vtype
	  ;; the variable type is unboxed, check for a more specific
	  ;; expression type
	  (let ((etype (j2s-type node)))
	     (cond
		((memq etype '(int32 uint32))
		 (if (m64? conf) 'int53 vtype))
		((eq? etype 'integer)
		 (if (m64? conf) 'int53 etype))
		(else
		 etype))))))

;*---------------------------------------------------------------------*/
;*    class-of ...                                                     */
;*    -------------------------------------------------------------    */
;*    Used to find the class of an X instanceof Y expression.          */
;*---------------------------------------------------------------------*/
(define (class-of rhs::J2SExpr)
   (cond
      ((isa? rhs J2SUnresolvedRef)
       (with-access::J2SUnresolvedRef rhs (id)
	  (case id
	     ((Array) 'array)
	     ((Argument) 'argument)
	     ((Date) 'date)
	     ((RegExp) 'regexp)
	     ((Object) 'object)
	     ((Function) 'function)
	     ((Promise) 'promise)
	     (else #f))))
      ((is-builtin-ref? rhs 'Object)
       'object)
      ((is-builtin-ref? rhs 'Array)
       'array)
      ((is-builtin-ref? rhs 'Date)
       'date)
      ((is-builtin-ref? rhs 'RegExp)
       'regexp)
      ((is-builtin-ref? rhs 'Function)
       'functionn)
      ((is-builtin-ref? rhs 'Promise)
       'promise)
      ((is-builtin-ref? rhs 'Argument)
       'argument)
      ((isa? rhs J2SClass)
       rhs)))

;*---------------------------------------------------------------------*/
;*    best-hint ...                                                    */
;*---------------------------------------------------------------------*/
(define (best-hint this::J2SExpr)
   (with-access::J2SExpr this (hint)
      (if (pair? hint)
	  (let loop ((hint (cdr hint))
		     (h (car hint)))
	     (cond
		((null? hint)
		 h)
		((>fx (cdr (car hint)) (cdr h))
		 (loop (cdr hint) (car hint)))
		((=fx (cdr (car hint)) (cdr h))
		 (loop (cdr hint) (cons '_ (cdr h))))
		(else
		 (loop (cdr hint) h))))
	  '(any . 0))))

;*---------------------------------------------------------------------*/
;*    is-hint? ...                                                     */
;*    -------------------------------------------------------------    */
;*    Is the most likely hint of type TYPE?                            */
;*---------------------------------------------------------------------*/
(define (is-hint? this::J2SExpr type)
   (eq? type (car (best-hint this))))

;*---------------------------------------------------------------------*/
;*    is-not-hint? ...                                                 */
;*    -------------------------------------------------------------    */
;*    Is hint unlikely                                                 */
;*---------------------------------------------------------------------*/
(define (is-not-hint? this::J2SExpr type)
   (with-access::J2SExpr this (hint)
      (let ((h (assq type hint)))
	 (when (pair? h)
	    (=fx (cdr h) (minvalfx))))))
   
;*---------------------------------------------------------------------*/
;*    assoc-method-type ...                                            */
;*    -------------------------------------------------------------    */
;*    A method entry is structured as follows:                         */
;*      1- the return type                                             */
;*      2- the type of the receiver                                    */
;*      3- a list of argument types: type or type*                     *
;*---------------------------------------------------------------------*/
(define (assoc-method-type name default methods)
   (let ((c (assoc name methods)))
      (if (pair? c) (cdr c) default)))

;*---------------------------------------------------------------------*/
;*    string-method-type ...                                           */
;*---------------------------------------------------------------------*/
(define (string-method-type name #!optional (default '(any any)))
   (assoc-method-type name default
      '(("charAt" . (string string index))
	("charCodeAt" . (number string index))
	("concat" . (string string string string string))
	("indexOf" . (indexof string index))
	("lastIndexOf" . (indexof string index))
	("localeCompare" . (integer string (string)))
	("naturalCompare" . (integer string (string)))
	("replace" . (string string (string regexp) (string function)))
	("search" . (indexof string regexp))
	("slice" . (string string index index))
	("split" . (array string (string regexp) index))
	("substr" . (string string index index))
	("substring" . (string string index index))
	("toLowerCase" . (string string))
	("toLocaleLowerCase" . (string string))
	("toUpperCase" . (string string))
	("toLocaleUpperCase" . (string string))
	("trim" . (string string)))))

;*---------------------------------------------------------------------*/
;*    string-static-method-type ...                                    */
;*---------------------------------------------------------------------*/
(define (string-static-method-type name #!optional (default '(any any)))
   (assoc-method-type name default
      '(("fromCharCode" . (string undefined integer)))))
   
;*---------------------------------------------------------------------*/
;*    math-static-method-type ...                                      */
;*---------------------------------------------------------------------*/
(define (math-static-method-type name #!optional (default '(any any)))
   (assoc-method-type name default
      '(("abs" . (anumber undefined number))
	("acos" . (real4 undefined real))
	("asin" . (real4 undefined real))
	("atan" . (real undefined real))
	("atan2" . (real undefined real))
	("ceil" . (number undefined real))
	("cos" . (real1 undefined real))
	("exp" . (number undefined real))
	("floor" . (number undefined real))
	("log" . (real undefined real))
	("max" . (anumber undefined number))
	("min" . (anumber undefined number))
	("pow" . (number undefined number))
	("random" . (ureal1 undefined))
	("round" . (number undefined real))
	("sin" . (real1 undefined real))
	("sqrt" . (real undefined real))
	("tan" . (real undefined real)))))

;*---------------------------------------------------------------------*/
;*    object-static-method-type ...                                    */
;*---------------------------------------------------------------------*/
(define (object-static-method-type name #!optional (default '(any any)))
   (assoc-method-type name default
      '(("keys" . (array object)))))
   
;*---------------------------------------------------------------------*/
;*    regexp-method-type ...                                           */
;*---------------------------------------------------------------------*/
(define (regexp-method-type name #!optional (default '(any any)))
   (assoc-method-type name default
      '(("test" . (bool regexp string)))))

;*---------------------------------------------------------------------*/
;*    map-method-type ...                                              */
;*---------------------------------------------------------------------*/
(define (map-method-type name #!optional (default '(any any)))
   (assoc-method-type name default
      '(("has" . (bool any)))))

;*---------------------------------------------------------------------*/
;*    weakmap-method-type ...                                          */
;*---------------------------------------------------------------------*/
(define (weakmap-method-type name #!optional (default '(any any)))
   (assoc-method-type name default
      '(("has" . (bool any)))))

;*---------------------------------------------------------------------*/
;*    number-method-type ...                                           */
;*---------------------------------------------------------------------*/
(define (number-method-type name #!optional (default '(any any)))
   (assoc-method-type name default
      '(("isInteger" . (bool number))
	("toString" . (string number number)))))

;*---------------------------------------------------------------------*/
;*    array-method-type ...                                            */
;*---------------------------------------------------------------------*/
(define (array-method-type name #!optional (default '(any any)))
   (assoc-method-type name default
      '(("concat" . (array array array array array))
	("every" . (bool array function))
	("filter" . (array array function))
	("find" . (any array function))
	("indexOf" . (indexof array index))
	("forEach" . (any array function))
	("join" . (string array string))
	("lastIndexOf" . (indexof array index))
	("map" . (array array function))
	("reduce" . (any array function))
	("reduceRight" . (any array function))
	("reverse" . (array array))
	("shift" . (any array))
	("slice" . (array array index index))
	("sort" . (array array function))
	("some" . (bool array function))
	("splice" . (array array index integer))
	("unshift" . (array array)))))

;*---------------------------------------------------------------------*/
;*    jsvector-method-type ...                                         */
;*---------------------------------------------------------------------*/
(define (jsvector-method-type name #!optional (default '(any any)))
   (assoc-method-type name default
      '(("concat" . (jsvector jsvector jsvector jsvector jsvector))
	("every" . (bool jsvector function))
	("filter" . (jsvector jsvector function))
	("find" . (any jsvector function))
	("indexOf" . (indexof jsvector index))
	("forEach" . (any jsvector function))
	("join" . (string jsvector string))
	("lastIndexOf" . (indexof jsvector index))
	("map" . (jsvector jsvector function))
	("reduce" . (any jsvector function))
	("reduceRight" . (any jsvector function))
	("reverse" . (jsvector jsvector))
	("shift" . (any jsvector))
	("slice" . (jsvector jsvector index index))
	("sort" . (jsvector jsvector function))
	("some" . (bool jsvector function))
	("splice" . (jsvector jsvector index integer))
	("unshift" . (jsvector jsvector)))))

;*---------------------------------------------------------------------*/
;*    find-builtin-method-type ...                                     */
;*---------------------------------------------------------------------*/
(define (find-builtin-method-type obj fn)

   (define (is-global? obj ident)
      (or (is-builtin-ref? obj ident)
	  (when (isa? obj J2SGlobalRef)
	     (with-access::J2SGlobalRef obj (id decl)
		(when (eq? id ident)
		   (not (decl-usage-has? decl '(assig))))))
	  (when (isa? obj J2SUnresolvedRef)
	     (with-access::J2SUnresolvedRef obj (id)
		(eq? id ident)))))
   
   (define (String? obj)
      (is-global? obj 'String))

   (define (Math? obj)
      (is-global? obj 'Math))

   (define (Object? obj)
      (is-global? obj 'Object))

   (case (j2s-type obj)
      ((string) (string-method-type fn))
      ((regexp) (regexp-method-type fn))
      ((map) (map-method-type fn))
      ((weakmap) (weakmap-method-type fn))
      ((number integer index) (number-method-type fn))
      ((array) (array-method-type fn))
      ((jsvector) (jsvector-method-type fn))
      ((unknown) '(unknown ()))
      (else
       (cond
	  ((String? obj) (string-static-method-type fn))
	  ((Math? obj) (math-static-method-type fn))
	  ((Object? obj) (object-static-method-type fn))
	  (else '(any any))))))

;*---------------------------------------------------------------------*/
;*    guess-builtin-method-type ...                                    */
;*---------------------------------------------------------------------*/
(define (guess-builtin-method-type obj fn)

   (define (is-global? obj ident)
      (when (isa? obj J2SGlobalRef)
	 (with-access::J2SGlobalRef obj (id decl)
	    (when (eq? id ident)
	       (not (decl-usage-has? decl '(assig)))))))
   
   (define (String? obj)
      (is-global? obj 'String))

   (define (Math? obj)
      (is-global? obj 'Math))

   (define (map-delete-duplicates l1 l2)
      ;; merge the two argument type lists
      (let loop ((l1 l1)
		 (l2 l2))
	 (cond
	    ((null? l1)
	     l2)
	    ((null? l2)
	     l1)
	    (else
	     (let ((l (cond
			 ((pair? (car l1))
			  (cond
			     ((pair? (car l2))
			      (delete-duplicates (append (car l1) (car l2))))
			     ((memq (car l2) (car l1))
			      (car l1))
			     (else
			      (cons (car l2) (car l1)))))
			 ((pair? (car l2))
			  (cond
			     ((memq (car l1) (car l2))
			      (car l2))
			     (else
			      (cons (car l1) (car l2)))))
			 ((eq? (car l1) (car l2))
			  (car l1))
			 (else
			  (list (car l1) (car l2))))))
		(cons l (loop (cdr l1) (cdr l2))))))))
	     
   (define (merge-candidate x y)
      (if (null? y)
	  x
	  (let ((ret (cond
			((pair? (car y))
			 (if (memq (car x) (car y))
			     (car y)
			     (cons (car x) (car y))))
			((eq? (car x) (car y))
			 (car x))
			(else
			 (list (car x) (car y)))))
		(self (cond
			 ((pair? (cadr y))
			  (if (memq (cadr x) (cadr y))
			      (cadr y)
			      (cons (cadr x) (cadr y))))
			 ((eq? (cadr x) (cadr y))
			  (cadr x))
			 (else
			  (list (cadr x) (cadr y)))))
		(args (map-delete-duplicates (cddr x) (cddr y))))
	     (cons* ret self args))))
      
   (define (guess-method obj fn)
      (let ((candidates (list
			  (string-method-type fn #f)
			  (regexp-method-type fn #f)
			  (number-method-type fn #f)
			  (array-method-type fn #f)
			  (jsvector-method-type fn #f)
			  (and (String? obj) (string-static-method-type fn #f))
			  (and (Math? obj) (math-static-method-type fn #f)))))
	 (let loop ((l candidates)
		    (res '()))
	    (cond
	       ((null? l)
		(if (pair? res) res '(any any)))
	       ((car l)
		(loop (cdr l) (merge-candidate (car l) res)))
	       (else
		(loop (cdr l) res))))))
   
   (let ((ty (j2s-type obj)))
      (if (memq ty '(unknown any))
	  (guess-method obj fn)
	  (find-builtin-method-type obj fn))))
   
;*---------------------------------------------------------------------*/
;*    is-builtin-ref? ...                                              */
;*---------------------------------------------------------------------*/
(define (is-builtin-ref? expr clazz)
   (cond
      ((isa? expr J2SUnresolvedRef)
       (with-access::J2SUnresolvedRef expr (id)
	  (eq? id clazz)))
      ((isa? expr J2SGlobalRef)
       (with-access::J2SGlobalRef expr (decl)
	  (with-access::J2SDecl decl (id)
	     (and (eq? id clazz) (not (decl-usage-has? decl '(assig)))))))
      ((isa? expr J2SRef)
       (with-access::J2SRef expr (decl)
	  (with-access::J2SDecl decl (id scope)
	     (and (eq? id clazz)
		  (or (and (memq scope '(%scope tls))
			   (not (decl-usage-has? decl '(ref assig set delete uninit rest eval))))
		      (and (isa? decl J2SDeclExtern)
			   (not (decl-usage-has? decl '(assig set eval)))))))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    constructor-only? ...                                            */
;*    -------------------------------------------------------------    */
;*    This predicates is #t iff the function is only used as a         */
;*    constructor.                                                     */
;*---------------------------------------------------------------------*/
(define (constructor-only?::bool decl::J2SDeclFun)
   (and (decl-usage-has? decl '(new))
	(not (decl-usage-has? decl '(ref assig call eval instanceof)))))

;*---------------------------------------------------------------------*/
;*    constructor-no-return? ...                                       */
;*    -------------------------------------------------------------    */
;*    Does a constructor return something else than UNDEF?             */
;*---------------------------------------------------------------------*/
(define (constructor-no-return? decl::J2SDeclFun)
   (let ((fun (j2sdeclinit-val-fun decl)))
      (when (isa? fun J2SFun)
	 (with-access::J2SFun fun (rtype)
	    (eq? rtype 'undefined)))))
	    
	     
