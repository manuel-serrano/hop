;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/utils.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 13 16:59:06 2013                          */
;*    Last change :  Sun Apr 22 09:14:54 2018 (serrano)                */
;*    Copyright   :  2013-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Utility functions                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_utils
   
   (import __js2scheme_ast
	   __js2scheme_dump)
   
   (export (pass ::bstring)
	   (error/loc proc obj msg loc)
	   (illegal-node ::bstring ::J2SNode)
	   (config-get ::pair-nil ::keyword #!optional def)
	   (config-get-mmap ::pair-nil path)
	   (config-put! ::pair-nil ::keyword ::obj)
	   (this?::bool ::J2SNode)
	   
	   (j2s-expression-src loc ::pair-nil ::bstring)
	   
	   (m64? conf)
	   (u32? conf)
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
	   (type-name type conf)
	   (min-type::symbol ::obj ::obj)
	   (max-type::symbol ::obj ::obj)
	   (js-uint32-tointeger expr conf)

	   (j2s-expr-type-test ::J2SExpr)

	   (j2s-type ::obj)
	   (j2s-vtype ::obj)
	   
	   (class-of ::J2SExpr)

	   (usage?::bool ::pair-nil ::pair-nil)
	   (only-usage?::bool ::pair-nil ::pair-nil)
	   (strict-usage?::bool ::pair-nil ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    pass ...                                                         */
;*---------------------------------------------------------------------*/
(define (pass name)
   (print name))

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
	 (j2s->list this)
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
   (or (type-integer? type) (eq? type 'number)))

;*---------------------------------------------------------------------*/
;*    type-object? ...                                                 */
;*---------------------------------------------------------------------*/
(define (type-object? type)
   (memq type '(object regexp date Promise array arguments global this)))

;*---------------------------------------------------------------------*/
;*    type-name ...                                                    */
;*---------------------------------------------------------------------*/
(define (type-name type conf)
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
      ((function) 'JsFunction)
      ((date) 'JsDate)
      ((string) 'obj)
      ((null) 'nil)
      ((String) 'JsString)
      ((Promise) 'JsPromise)
      ((class) 'JsFunction)
      ((arguments) 'JsArguments)
      (else type)))
   
;*---------------------------------------------------------------------*/
;*    min-type ...                                                     */
;*    -------------------------------------------------------------    */
;*    Return the smallest type that can represent both types.          */
;*---------------------------------------------------------------------*/
(define (min-type t1 t2)
   (if (eq? t1 t2)
       t1
       (case t1
	  ((index) t1)
	  ((length) (if (eq? t2 'index) 'index t1))
	  ((int32) t1)
	  ((uint32) (if (memq t2 '(index length)) t2 t1))
	  ((int53) (if (eq? t2 'int32) t2 t2))
	  ((integer) (if (memq t2 '(int32 uint32)) t2 t1))
	  ((number integer) t1)
	  (else 'any))))

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
			 (values op decl (string->symbol val) expr)))))))))
   
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
		 (when (isa? lhs J2SRef)
		    (with-access::J2SRef lhs (decl)
		       (values op decl 'null lhs))))
		((isa? rhs J2SUndefined)
		 (when (isa? lhs J2SRef)
		    (with-access::J2SRef lhs (decl)
		       (values op decl 'undefined lhs))))
		((isa? lhs J2SNull)
		 (when (isa? rhs J2SRef)
		    (with-access::J2SRef rhs (decl)
		       (values op decl 'null rhs))))
		((isa? lhs J2SUndefined)
		 (when (isa? rhs J2SRef)
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
	  (let ((typ (case (native-type-test expr)
			((js-index?) 'index)
			((fixnum?) 'integer)
			((number?) 'number)
			((js-jsstring?) 'string)
			((js-array?) 'array)
			((js-object?) 'object)
			((js-function?) 'function)
			((boolean?) 'bool)
			((js-undefined?) 'undefined)
			((js-null?) 'null)
			(else #f))))
	     (if typ
		 (with-access::J2SRef ref (decl)
		    (values '== decl typ ref))
		 #f))))
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
       (with-access::J2SHopRef node (vtype)
	  vtype))
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
;*    class-of ...                                                     */
;*    -------------------------------------------------------------    */
;*    Used to find the class of an X instanceof Y expression.          */
;*---------------------------------------------------------------------*/
(define (class-of rhs::J2SExpr)
   (when (isa? rhs J2SUnresolvedRef)
      (with-access::J2SUnresolvedRef rhs (id)
	 (case id
	    ((Array) 'array)
	    ((Argument) 'argument)
	    ((Date) 'date)
	    ((RegExp) 'regexp)
	    ((Object) 'object)
	    ((Function) 'function)
	    ((Promise) 'promise)
	    (else 'unknown)))))

;*---------------------------------------------------------------------*/
;*    usage? ...                                                       */
;*---------------------------------------------------------------------*/
(define (usage? keys usage)
   (any (lambda (k) (memq k usage)) keys))

;*---------------------------------------------------------------------*/
;*    only-usage? ...                                                  */
;*---------------------------------------------------------------------*/
(define (only-usage? keys usage)
   (every (lambda (u) (memq u keys)) usage))

;*---------------------------------------------------------------------*/
;*    strict-usage? ...                                                */
;*---------------------------------------------------------------------*/
(define (strict-usage? keys usage)
   (and (=fx (length keys) (length usage))
	(only-usage? keys usage)))
	
	
