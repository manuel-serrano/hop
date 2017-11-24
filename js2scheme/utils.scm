;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/utils.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 13 16:59:06 2013                          */
;*    Last change :  Fri Nov 24 10:25:35 2017 (serrano)                */
;*    Copyright   :  2013-17 Manuel Serrano                            */
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
	   (config-put! ::pair-nil ::keyword ::obj)
	   (this?::bool ::J2SNode)
	   
	   (j2s-expression-src loc ::pair-nil ::bstring)
	   
	   (type-int32?::bool ::obj)
	   (type-uint32?::bool ::obj)
	   (type-int30?::bool ::obj)
	   (type-int53?::bool ::obj)
	   (type-fixnum?::bool ::obj)
	   (type-integer?::bool ::obj)
	   (type-number?::bool ::obj)
	   (type-name type conf)
	   (minimal-type::symbol ::obj ::obj)
	   (max-type::symbol ::obj ::obj)
	   (js-uint32->jsnum expr conf)
	   (js-uint32->fixnum expr conf)
	   (js-fixnum->uint32 expr)))

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
	     
   (let ((m (config-get conf :mmap-src)))
      (if (mmap? m)
	  (match-case loc
	     ((at ?path ?start)
	      (mmap-substring m
		 (fixnum->elong start) (find-delim m (fixnum->elong start))))
	     (else
	      default))
	  default)))

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
   (memq type '(uint29 int30 fixnum ufixnum)))

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
;*    type-name ...                                                    */
;*---------------------------------------------------------------------*/
(define (type-name type conf)
   
   (define (m64? conf)
      (=fx (config-get conf :long-size 0) 64))
   
   (case type
      ((int32 int53) (if (m64? conf) 'long 'obj))
      ((unknown any number) 'obj)
      ((index uint32 length) 'uint32)
      ((uint29 int30 fixnum ufixnum) 'long)
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
      (else type)))
   
;*---------------------------------------------------------------------*/
;*    minimal-type ...                                                 */
;*---------------------------------------------------------------------*/
(define (minimal-type t1 t2)
   (if (eq? t1 t2)
       t1
       (case t1
	  ((uint29)
	   t2)
	  ((index)
	   (if (memq t2 '(uint29)) t1 t2))
	  ((length)
	   (if (memq t2 '(uint29 index)) t1 t2))
	  ((uint32)
	   (if (memq t2 '(uint29 index length)) t1 t2))
	  ((int30 fixnum ufixnum)
	   (if (memq t2 '(uint29))
	       t1
	       (if (type-integer? t2) 'integer 'number)))
	  ((integer)
	   (if (type-integer? t2) 'integer 'number))
	  ((number)
	   (if (type-number? t2) 'number 'any))
	  (else 'any))))

;*---------------------------------------------------------------------*/
;*    max-type ...                                                     */
;*    -------------------------------------------------------------    */
;*    Return the biggest type that can represent both types.           */
;*---------------------------------------------------------------------*/
(define (max-type left right)
   (cond
      ((eq? left right)
       left)
      ((eq? left 'uint29)
       (case right
	  ((int30 index length integer number) right)
	  (else 'any)))
      ((eq? left 'int30)
       (case right
	  ((uint29) left)
	  ((index length integer number) right)
	  (else 'any)))
      ((eq? left 'index)
       (case right
	  ((uint29) left)
	  ((length integer number) right)
	  (else 'any)))
      ((eq? left 'length)
       (case right
	  ((uint29 index) left)
	  ((integer number) right)
	  (else 'any)))
      ((or (eq? left 'integer) (eq? left 'int53))
       (case right
	  ((uint29 index int30 int53) left)
	  ((number) right)
	  (else 'any)))
      (else
       'any)))

;*---------------------------------------------------------------------*/
;*    js-uint32->jsnum ...                                             */
;*---------------------------------------------------------------------*/
(define (js-uint32->jsnum expr conf)
   (let ((lgsz (config-get conf :long-size 30)))
      (cond
	 ((and (uint32? expr) (<u32 expr (bit-lshu32 #u32:1 (-fx lgsz 1))))
	  (uint32->fixnum expr))
	 ((>fx lgsz 32)
	  `(uint32->fixnum ,expr))
	 (else
	  `(js-uint32->jsnum ,expr)))))

;*---------------------------------------------------------------------*/
;*    js-uint32->fixnum ...                                            */
;*---------------------------------------------------------------------*/
(define (js-uint32->fixnum expr conf)
   (cond
      ((uint32? expr)
       (uint32->fixnum expr))
      (else
       `(uint32->fixnum ,expr))))

;*---------------------------------------------------------------------*/
;*    js-fixnum->uint32 ...                                            */
;*---------------------------------------------------------------------*/
(define (js-fixnum->uint32 expr)
   (if (fixnum? expr)
       (fixnum->uint32 expr)
       `(fixnum->uint32 ,expr)))

