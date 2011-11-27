;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-11 Florian Loitsch, see LICENSE file         */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

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

(define-inline (make-eq-hashtable #!optional (size #unspecified))
   (make-hashtable size #unspecified eq?))

(define (eq-hashtable-clone ht)
   (let ((cloned-ht (make-eq-hashtable)))
      (hashtable-for-each ht
			  (lambda (key val)
			     (hashtable-put! cloned-ht key val)))
      cloned-ht))

(define (hashtable-append! ht1 ht2)
   (hashtable-for-each ht2
		       (lambda (key val)
			  (hashtable-put! ht1 key val))))

(define-macro (begin0 . L)
   (let ((fst (gensym 'fst)))
      `(let ((,fst ,(car L)))
	  ,@(cdr L)
	  ,fst)))

(define-macro (cons-set! lvalue val)
   `(set! ,lvalue (cons ,val (or ,lvalue '()))))

(define-macro (cp-filter . L)
   `(map (lambda (x) x)
	 (filter ,@L)))

(define-macro (for i from to . Lbody)
   (let ((loop (gensym 'loop))
	 (to-tmp (gensym 'to)))
      `(let ((,to-tmp ,to))
	  (let ,loop ((,i ,from))
	       (when (< ,i ,to-tmp)
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

