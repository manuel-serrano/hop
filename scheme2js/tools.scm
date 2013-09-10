;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/scheme2js/tools.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  2007-12                                           */
;*    Last change :  Fri Aug  2 09:59:42 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Scheme2js misc tools                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module tools
   (export (inline make-eq-hashtable #!optional (size #unspecified))
	   (eq-hashtable-clone ht)
	   (hashtable-append! ht1 ht2)
	   (macro begin0)
	   (macro cons-set!)
	   (macro cp-filter)
	   (macro for)
	   (make-typed-ident::symbol ::symbol ::symbol)
	   (id-of-id::symbol ::symbol)
	   (parse-ident ::symbol)))

;*---------------------------------------------------------------------*/
;*    make-eq-hashtable ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (make-eq-hashtable #!optional (size #unspecified))
   (make-hashtable size #unspecified eq?))

;*---------------------------------------------------------------------*/
;*    eq-hashtable-clone ...                                           */
;*---------------------------------------------------------------------*/
(define (eq-hashtable-clone ht)
   (let ((cloned-ht (make-eq-hashtable)))
      (hashtable-for-each ht
	 (lambda (key val)
	    (hashtable-put! cloned-ht key val)))
      cloned-ht))

;*---------------------------------------------------------------------*/
;*    hashtable-append! ...                                            */
;*---------------------------------------------------------------------*/
(define (hashtable-append! ht1 ht2)
   (hashtable-for-each ht2
      (lambda (key val)
	 (hashtable-put! ht1 key val))))

;*---------------------------------------------------------------------*/
;*    begin0                                                           */
;*---------------------------------------------------------------------*/
(define-macro (begin0 . L)
   (let ((fst (gensym 'fst)))
      `(let ((,fst ,(car L)))
	  ,@(cdr L)
	  ,fst)))

;*---------------------------------------------------------------------*/
;*    cons-set! ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (cons-set! lvalue val)
   `(set! ,lvalue (cons ,val (or ,lvalue '()))))

;*---------------------------------------------------------------------*/
;*    cp-filter ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (cp-filter . L)
   `(map (lambda (x) x)
	 (filter ,@L)))

;*---------------------------------------------------------------------*/
;*    for ...                                                          */
;*---------------------------------------------------------------------*/
(define-macro (for i from to . Lbody)
   (let ((loop (gensym 'loop))
	 (to-tmp (gensym 'to)))
      `(let ((,to-tmp ,to))
	  (let ,loop ((,i ,from))
	       (when (<fx ,i ,to-tmp)
		  ,@Lbody
		  (,loop (+fx ,i 1)))))))

;*---------------------------------------------------------------------*/
;*    id-of-id ...                                                     */
;*---------------------------------------------------------------------*/
(define (id-of-id v::symbol)
   (let* ((str (symbol->string! v))
	  (pos (string-contains str "::")))
      (if pos
	  (string->symbol (substring str 0 pos))
	  v)))

;*---------------------------------------------------------------------*/
;*    parse-ident ...                                                  */
;*---------------------------------------------------------------------*/
(define (parse-ident v::symbol)
   (let* ((str (symbol->string! v))
	  (pos (string-contains str "::")))
      (if pos
	  (values (string->symbol (substring str 0 pos))
	     (string->symbol (substring str (+fx pos 2)
				(string-length str))))
	  (values v #f))))

;*---------------------------------------------------------------------*/
;*    4dots ...                                                        */
;*---------------------------------------------------------------------*/
(define 4dots (string->symbol "::"))

;*---------------------------------------------------------------------*/
;*    make-typed-ident ...                                             */
;*---------------------------------------------------------------------*/
(define (make-typed-ident sym1 sym2)
   (symbol-append sym1 4dots sym2))

