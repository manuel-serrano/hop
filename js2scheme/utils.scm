;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/utils.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 13 16:59:06 2013                          */
;*    Last change :  Mon Jan 23 09:36:29 2017 (serrano)                */
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
	   
	   (type-uint32?::bool ::symbol)
	   (type-int30?::bool ::symbol)
	   (type-integer?::bool ::symbol)
	   (type-number?::bool ::symbol)
	   (type-name type conf)
	   (minimal-type::symbol ::obj ::obj)
	   (max-type::symbol ::obj ::obj)))

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
;*    type-uint32? ...                                                 */
;*---------------------------------------------------------------------*/
(define (type-uint32? type)
   (memq type '(uint29 index uint32 length)))

;*---------------------------------------------------------------------*/
;*    type-int30? ...                                                  */
;*---------------------------------------------------------------------*/
(define (type-int30? type)
   (memq type '(uint29 int30)))

;*---------------------------------------------------------------------*/
;*    type-int53? ...                                                  */
;*---------------------------------------------------------------------*/
(define (type-int53? type)
   (memq type '(uint29 int30 int53)))

;*---------------------------------------------------------------------*/
;*    type-integer? ...                                                */
;*---------------------------------------------------------------------*/
(define (type-integer? type::symbol)
   (or (type-int53? type)  (type-uint32? type) (memq type '(integer fixnum))))
   
;*---------------------------------------------------------------------*/
;*    type-number? ...                                                 */
;*---------------------------------------------------------------------*/
(define (type-number? type::symbol)
   (or (type-integer? type) (eq? type 'number)))

;*---------------------------------------------------------------------*/
;*    type-name ...                                                    */
;*---------------------------------------------------------------------*/
(define (type-name type conf)
   (case type
      ((unknown any number) 'obj)
      ((uint29 index uint32 length) 'uint32)
      ((int30 fixnum) 'long)
      ((integer int53) (if (=fx (config-get conf :long-size 0) 64) 'long 'obj))
      ((object) 'JsObject)
      ((undefined) 'unspecified)
      ((regexp) 'JsRegExp)
      ((array) 'JsArray)
      ((function) 'JsFunction)
      ((date) 'JsDate)
      ((string) 'obj)
      ((null) 'nil)
      (else type)))
   
;*---------------------------------------------------------------------*/
;*    minimal-type ...                                                 */
;*---------------------------------------------------------------------*/
(define (minimal-type utype type)
   (if (eq? utype type)
       utype
       (case utype
	  ((uint29)
	   type)
	  ((index)
	   (if (memq type '(uint29)) utype type))
	  ((length)
	   (if (memq type '(uint29 index)) utype type))
	  ((uint32)
	   (if (memq type '(uint29 index length)) utype type))
	  ((int30 fixnum)
	   (if (memq type '(uint29))
	       utype
	       (if (type-integer? type) 'integer 'number)))
	  ((integer)
	   (if (type-integer? type) 'integer 'number))
	  ((number)
	   (if (type-number? type) 'number 'any))
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
